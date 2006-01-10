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
                                   (file-name-directory file))))

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
  (if (not (file-exists-p (concat dir "/" (svn-svn-wc-adm-dir-name) "/")))
      (when (y-or-n-p
             (concat dir
                     " does not seem to be a Subversion working copy (no "
                     (svn-svn-wc-adm-dir-name) " directory).  "
                     "Run dired instead? "))
        (dired dir))
    (setq dir (file-name-as-directory dir))
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
        (setq default-directory dir
              svn-status-remote (when arg t))
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

(provide 'psvn-svn)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; psvn-svn.el ends here
