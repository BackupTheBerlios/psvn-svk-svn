;;; psvn-svn.el --- Subversion support for psvn.el / Emacs
;; Copyright (C) 2002-2005 by Stefan Reichoer & intrigeri

;; Authors:
;;   - Stefan Reichoer, <stefan@xsteve.at>
;;   - Ben Voui, <intrigeri@boum.org>

;; Contains some code from VC, under GPL:
;;   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1998, 1999, 2000, 2002,
;;   2003, 2004, 2005 Free Software Foundation, Inc.

;; $Id$
;; $URL$

;; psvn-svn.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; psvn-svn.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defun svn-svn-registered (file)
  "Return true if FILE is registered under Subversion."
  ;; a quick false positive test: is there a `.svn/entries' file?
  (file-exists-p (expand-file-name (concat (svn-svn-wc-adm-dir-name) "/entries")
                                   (file-name-directory (concat file "/")))))

;; named after SVN_WC_ADM_DIR_NAME in svn_wc.h
(defun svn-svn-wc-adm-dir-name ()
  "Return the name of the \".svn\" subdirectory or equivalent."
  (if (and (eq system-type 'windows-nt)
           (getenv "SVN_ASP_DOT_NET_HACK"))
      "_svn"
    ".svn"))

;;;###autoload
(defun svn-svn-status (dir &optional arg)
  "Examine the status of Subversion working copy in directory DIR.
If ARG then pass the -u argument to `svn status'."
  (setq arg (svn-status-possibly-negate-meaning-of-arg arg 'svn-status))
  (unless (file-directory-p dir)
    (error "%s is not a directory" dir))
  (if (not (svn-svn-registered dir))
      (when (y-or-n-p
             (concat dir
                     " does not seem to be a Subversion working copy (no "
                     (svn-svn-wc-adm-dir-name) " directory).  "
                     "Run dired instead? "))
        (dired dir))
    (setq dir (file-name-as-directory dir))
    (setq default-directory dir)
    (when svn-status-load-state-before-svn-status
      (unless (string= dir (car svn-status-directory-history))
        (svn-status-load-state t)))
    (setq svn-status-directory-history (delete dir svn-status-directory-history))
    (add-to-list 'svn-status-directory-history dir)
    (if (string= (buffer-name) svn-status-buffer-name)
        (setq svn-status-display-new-status-buffer nil)
      (setq svn-status-display-new-status-buffer t)
      ;;(message "psvn: Saving initial window configuration")
      (setq svn-status-initial-window-configuration (current-window-configuration)))
    (let* ((status-buf (get-buffer-create svn-status-buffer-name))
           (proc-buf (get-buffer-create "*svn-process*"))
           (status-option (if svn-status-verbose
                              (if arg "-uv" "-v")
                            (if arg "-u" ""))))
      (save-excursion
        (set-buffer status-buf)
        (setq default-directory dir)
        (set-buffer proc-buf)
        (setq default-directory dir)
        (setq svn-status-remote (when arg t))
        (svn-run-svn t t 'status "status" status-option)))))

(defun svn-svn-run (run-asynchron clear-process-buffer cmdtype &rest arglist)
  "Run svn with arguments ARGLIST.

If RUN-ASYNCHRON is t then run svn asynchronously.

If CLEAR-PROCESS-BUFFER is t then erase the contents of the
*svn-process* buffer before commencing.

CMDTYPE is a symbol such as 'mv, 'revert, or 'add, representing the
command to run.

ARGLIST is a list of arguments \(which must include the command name,
for example: '(\"revert\" \"file1\"\)
ARGLIST is flattened and any every nil value is discarded.

If the variable `svn-status-edit-svn-command' is non-nil then the user
is prompted for give extra arguments, which are appended to ARGLIST."
  (setq arglist (svn-status-flatten-list arglist))
  (if (eq (process-status "svn") nil)
      (progn
        (when svn-status-edit-svn-command
          (setq arglist (append arglist
                                (split-string
                                 (read-from-minibuffer
                                  (format "Run `svn %s' with extra arguments: "
                                          (mapconcat 'identity arglist " "))))))
          (when (eq svn-status-edit-svn-command t)
            (svn-status-toggle-edit-cmd-flag t))
          (message "svn-run-svn %s: %S" cmdtype arglist))
        (let* ((proc-buf (get-buffer-create "*svn-process*"))
               (svn-exe svn-status-svn-executable)
               (svn-proc))
          (when (listp (car arglist))
            (setq arglist (car arglist)))
          (save-excursion
            (set-buffer proc-buf)
            (when svn-status-coding-system
              (setq buffer-file-coding-system svn-status-coding-system))
            (setq buffer-read-only nil)
            (fundamental-mode)
            (if clear-process-buffer
                (delete-region (point-min) (point-max))
              (goto-char (point-max)))
            (setq svn-process-cmd cmdtype)
            (setq svn-status-mode-line-process-status (format " running %s" cmdtype))
            (svn-status-update-mode-line)
            (sit-for 0.1)
            (if run-asynchron
                (progn
                  ;;(message "running asynchron: %s %S" svn-exe arglist)
                  (let ((process-environment (svn-process-environment))
                        (process-connection-type nil))
                    ;; Communicate with the subprocess via pipes rather
                    ;; than via a pseudoterminal, so that if the svn+ssh
                    ;; scheme is being used, SSH will not ask for a
                    ;; passphrase via stdio; psvn.el is currently unable
                    ;; to answer such prompts.  Instead, SSH will run
                    ;; x11-ssh-askpass if possible.  If Emacs is being
                    ;; run on a TTY without $DISPLAY, this will fail; in
                    ;; such cases, the user should start ssh-agent and
                    ;; then run ssh-add explicitly.
                    (setq svn-proc (apply 'start-process "svn" proc-buf svn-exe arglist)))
                  (set-process-sentinel svn-proc 'svn-process-sentinel)
                  (when svn-status-track-user-input
                    (set-process-filter svn-proc 'svn-process-filter)))
              ;;(message "running synchron: %s %S" svn-exe arglist)
              (let ((process-environment (svn-process-environment)))
                ;; `call-process' ignores `process-connection-type' and
                ;; never opens a pseudoterminal.
                (apply 'call-process svn-exe nil proc-buf nil arglist))
              (setq svn-status-mode-line-process-status "")
              (svn-status-update-mode-line)))))
    (error "You can only run one svn process at once!")))

(defun svn-svn-status-parse-ar-output ()
  "Parse the output of svn add|remove.
Return a list that is suitable for `svn-status-update-with-command-list'"
  (save-excursion
    (set-buffer "*svn-process*")
    (let ((action)
          (name)
          (skip)
          (result))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (cond ((= (svn-point-at-eol) (svn-point-at-bol)) ;skip blank lines
               (setq skip t))
              ((looking-at "A")
               (setq action 'added-wc))
              ((looking-at "D")
               (setq action 'deleted-wc))
              (t ;; this should never be needed(?)
               (setq action 'unknown)))
        (unless skip ;found an interesting line
          (forward-char 10)
          (setq name (buffer-substring-no-properties (point) (svn-point-at-eol)))
          (setq result (cons (list name action)
                             result))
          (setq skip nil))
        (forward-line 1))
      result)))
;;(svn-status-parse-ar-output)
;; (svn-status-update-with-command-list (svn-status-parse-ar-output))

(defun svn-svn-status-parse-info-result ()
  (let ((url))
    (save-excursion
      (set-buffer "*svn-process*")
      (goto-char (point-min))
      (let ((case-fold-search t))
        (search-forward "url: "))
      (setq url (buffer-substring-no-properties (point) (svn-point-at-eol))))
    (setq svn-status-base-info `((url ,url)))))

(defun svn-svn-status-show-svn-log (arg)
  "Run `svn log' on selected files.
The output is put into the *svn-log* buffer
The optional prefix argument ARG determines which switches are passed to `svn log':
 no prefix               --- use whatever is in the list `svn-status-default-log-arguments'
 prefix argument of -1   --- use no arguments
 prefix argument of 0:   --- use the -q switch (quiet)
 other prefix arguments: --- use the -v switch (verbose)

See `svn-status-marked-files' for what counts as selected."
  (let ((switches (cond ((eq arg 0)  '("-q"))
                        ((eq arg -1) '())
                        (arg         '("-v"))
                        (t           svn-status-default-log-arguments))))
    (svn-status-create-arg-file svn-status-temp-arg-file "" (svn-status-marked-files) "")
    (svn-run-svn t t 'log "log" "--targets" svn-status-temp-arg-file switches)
    (save-excursion
      (set-buffer "*svn-process*")
      (svn-log-view-mode))))

(defun svn-svn-status-svnversion ()
  "Run svnversion on the directory that contains the file at point."
  (svn-status-ensure-cursor-on-file)
  (let ((simple-path (svn-status-line-info->filename (svn-status-get-line-information)))
        (full-path (svn-status-line-info->full-path (svn-status-get-line-information)))
        (version))
    (unless (file-directory-p simple-path)
      (setq simple-path (or (file-name-directory simple-path) "."))
      (setq full-path (file-name-directory full-path)))
    (setq version (shell-command-to-string (concat "svnversion -n " full-path)))
    (message "svnversion for '%s': %s" simple-path version)
    version))

;; --------------------------------------------------------------------------------
;; status persistent options
;; --------------------------------------------------------------------------------

(defun svn-svn-status-base-dir (&optional file)
  (let ((base-dir (or (and file (file-name-directory (concat file "/")))
                      (expand-file-name default-directory)))
        (dot-svn-dir)
        (dir-below (expand-file-name default-directory)))
    (setq dot-svn-dir (concat base-dir (svn-svn-wc-adm-dir-name)))
    (while (when (and dir-below (file-exists-p dot-svn-dir))
             (setq base-dir (file-name-directory dot-svn-dir))
             (string-match "\\(.+/\\).+/" dir-below)
             (setq dir-below (match-string 1 dir-below))
             (setq dot-svn-dir (concat dir-below (svn-svn-wc-adm-dir-name)))))
    base-dir))

(provide 'psvn-svn)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; psvn-svn.el ends here
