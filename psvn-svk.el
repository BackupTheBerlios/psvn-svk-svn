;;; psvn-svk.el --- SVK support for psvn.el / Emacs
;; Copyright (C) 2002-2005 by Stefan Reichoer & intrigeri

;; Author: intrigeri <intrigeri@boum.org>
;; $Id$
;; $URL$

;; psvn-svk.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; psvn-svk.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; init

; better keep 'SVN first
(add-to-list 'svn-handled-backends 'SVK t)

;;; Compatibility with Emacs <22

(if (fboundp 'time-less-p)
    (defalias 'svn-svk-time-less-p 'time-less-p)
  (defun svn-svk-time-less-p (t1 t2)
    "Say whether time value T1 is less than time value T2."
    (with-decoded-time-value ((high1 low1 micro1 t1)
                              (high2 low2 micro2 t2))
      (or (< high1 high2)
          (and (= high1 high2)
               (or (< low1 low2)
                   (and (= low1 low2)
                        (< micro1 micro2))))))))

(if (fboundp 'assoc-string)
    (defalias 'svn-svk-assoc-string 'assoc-string)
  (defun svn-svk-assoc-string (key alist)
    (assoc-default key alist
                   (lambda (a b)
                     (and (stringp a) (stringp b) (string-equal a b))))))


;;; Functions needed by psvn interface

(defun svn-svk-registered (file)
  "Check if FILE is SVK registered."
  (let ((lfile (file-truename file)))   ; SVK stores truenames
    (svn-svk-co-path-p lfile)))

;;;###autoload
(defun svn-svk-status (dir &optional arg)
  "Examine the status of SVK working copy in directory DIR."
  (setq arg (svn-status-possibly-negate-meaning-of-arg arg 'svn-status))
  (unless (file-directory-p dir)
    (error "%s is not a directory" dir))
  (if (not (svn-svk-registered dir))
      (when (y-or-n-p
             (concat dir " does not seem to be a SVK working copy. "
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
      (setq svn-status-initial-window-configuration (current-window-configuration)))
    (let* ((status-buf (get-buffer-create svn-status-buffer-name))
           (proc-buf (get-buffer-create "*svn-process*"))
           (status-option (if svn-status-verbose "-v" "")))
      (save-excursion
        (set-buffer status-buf)
        (setq default-directory dir)
        (set-buffer proc-buf)
        (setq default-directory dir
              svn-status-remote (when arg t))
        (svn-svk-run t t 'status "status" status-option)))))

(defun svn-svk-run (run-asynchron clear-process-buffer cmdtype &rest arglist)
  "Run svk with arguments ARGLIST.

If RUN-ASYNCHRON is t then run svk asynchronously.

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
  (if (eq (process-status "svk") nil)
      (progn
        (when svn-status-edit-svn-command
          (setq arglist (append arglist
                                (split-string
                                 (read-from-minibuffer
                                  (format "Run `svk %s' with extra arguments: "
                                          (mapconcat 'identity arglist " "))))))
          (when (eq svn-status-edit-svn-command t)
            (svn-status-toggle-edit-cmd-flag t))
          (message "svn-svk-run %s: %S" cmdtype arglist))
        (let* ((proc-buf (get-buffer-create "*svn-process*"))
               (svn-exe svn-status-svk-executable)
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
                  (message "running asynchron: %s %S" svn-exe arglist)
                  (sit-for 7)
                  (let ((process-environment (svn-process-environment))
                        (process-connection-type nil))
                    ;; Communicate with the subprocess via pipes rather
                    ;; than via a pseudoterminal, so that if the svk+ssh
                    ;; scheme is being used, SSH will not ask for a
                    ;; passphrase via stdio; psvn.el is currently unable
                    ;; to answer such prompts.  Instead, SSH will run
                    ;; x11-ssh-askpass if possible.  If Emacs is being
                    ;; run on a TTY without $DISPLAY, this will fail; in
                    ;; such cases, the user should start ssh-agent and
                    ;; then run ssh-add explicitly.
                    (setq svn-proc (apply 'start-process "svk" proc-buf svn-exe arglist)))
                  (set-process-sentinel svn-proc 'svn-process-sentinel)
                  (when svn-status-track-user-input
                    (set-process-filter svn-proc 'svn-process-filter)))
              (message "running synchron: %s %S" svn-exe arglist)
              (let ((process-environment (svn-process-environment)))
                ;; `call-process' ignores `process-connection-type' and
                ;; never opens a pseudoterminal.
                (apply 'call-process svn-exe nil proc-buf nil arglist))
              (setq svn-status-mode-line-process-status "")
              (svn-status-update-mode-line)))))
    (error "You can only run one svk process at once!")))


;;; Aux. functions that will often avoid slow calls to svk.

(defvar svn-svk-co-paths nil)
(defun svn-svk-co-paths ()
  (interactive)
  (let ((config "~/.svk/config")
        mtime)
    (when (file-readable-p config)
      (setq mtime (nth 5 (file-attributes "~/.svk/config")))
      (unless (and svn-svk-co-paths           ; has not it been loaded?
                   (svn-svk-time-less-p mtime ; is it unmodified since?
                                       (car (last svn-svk-co-paths))))
        ;; (re)load
        (setq svn-svk-co-paths (list mtime))
        (with-temp-buffer
          (insert-file-contents config)
          (when (search-forward "hash:\n" nil t) ; to start of co paths
            (while (re-search-forward               ; to next co path
                    "^ +\\(/.*\\):\n.*depotpath: \\(/.+\\)$" nil t)
              (add-to-list 'svn-svk-co-paths
                           (list (match-string-no-properties 1)
                                 (match-string-no-properties 2)))))))))
  svn-svk-co-paths)

(defun svn-svk-co-path-p (file)
  "Whether SVK manages a parent directory of FILE.
Note that this does not try to guarantee SVK manages this particular
subdirectory. That's for the full `svn-svk-registered' to decide."
  (svn-svk-co-paths)
  (block nil
    (unless (file-exists-p file)
      (return nil))
    ;; Check file and each parent dir for svk-ness
    ;; Yeah, this is not the greatest. And it's UNIX-centric.
    (while (and file (not (string-equal file "/")))
      ;; For both SVK and file-name-directory, dirnames must not
      ;; include trailing /
      (setq file (substring file 0 (string-match "/\\'" file)))
      (if (svn-svk-assoc-string file svn-svk-co-paths)
          (return t)
        (setq file (file-name-directory file))))))

(defun svn-svk-co-path-of (file)
  "Return the CO path holding FILE, or nil."
  (car (find-if #'(lambda (codir)
                    (and (stringp codir)
                         (string-match (concat "^" codir) file)))
                svn-svk-co-paths
                :key 'first)))

(provide 'psvn-svk)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; psvn-svk.el ends here
