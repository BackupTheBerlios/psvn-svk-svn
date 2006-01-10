;;; psvn-svn.el --- Subversion support for psvn.el / Emacs
;; Copyright (C) 2002-2006 by Stefan Reichoer
;; $Id$
;; $URL$

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
  "Implementation of `svn-status' for the SVN backend."
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
        (setq default-directory dir
              svn-status-remote (when arg t))
        (svn-svn-run t t 'status "status" status-option)))))

(defun svn-svn-run (run-asynchron clear-process-buffer cmdtype &rest arglist)
  "Implementation of `svn-run' for the SVN backend."
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
          (message "svn-svn-run %s: %S" cmdtype arglist))
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

(defun svn-svn-status-parse-commit-output ()
  "Implementation of `svn-status-parse-commit-output' for the SVN backend."
  (save-excursion
    (set-buffer "*svn-process*")
    (let ((action)
          (name)
          (skip)
          (result))
      (goto-char (point-min))
      (setq svn-status-commit-rev-number nil)
      (setq skip nil) ; set to t whenever we find a line not about a committed file
      (while (< (point) (point-max))
        (cond ((= (svn-point-at-eol) (svn-point-at-bol)) ;skip blank lines
               (setq skip t))
              ((looking-at "Sending")
               (setq action 'committed))
              ((looking-at "Adding")
               (setq action 'added))
              ((looking-at "Deleting")
               (setq action 'deleted))
              ((looking-at "Transmitting file data")
               (setq skip t))
              ((looking-at "Committed revision \\([0-9]+\\)")
               (setq svn-status-commit-rev-number
                     (string-to-number (svn-match-string-no-properties 1)))
               (setq skip t))
              (t ;; this should never be needed(?)
               (setq action 'unknown)))
        (unless skip                                ;found an interesting line
          (forward-char 15)
          (when svn-status-operated-on-dot
            ;; when the commit used . as argument, delete the trailing directory
            ;; from the svn output
            (search-forward "/" nil t))
          (setq name (buffer-substring-no-properties (point) (svn-point-at-eol)))
          (setq result (cons (list name action)
                             result))
          (setq skip nil))
        (forward-line 1))
      result)))

(defun svn-svn-status-parse-ar-output ()
  "Implementation of `svn-status-parse-ar-output' for the SVN backend."
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
  "Implementation of `svn-status-parse-info-result' for the SVN backend."
  (let ((url))
    (save-excursion
      (set-buffer "*svn-process*")
      (goto-char (point-min))
      (let ((case-fold-search t))
        (search-forward "url: "))
      (setq url (buffer-substring-no-properties (point) (svn-point-at-eol))))
    (setq svn-status-base-info `((url ,url)))))

(defun svn-svn-status-show-svn-log (arg)
  "Implementation of `svn-status-show-svn-log' for the SVN backend."
  (let ((switches (cond ((eq arg 0)  '("-q"))
                        ((eq arg -1) '())
                        (arg         '("-v"))
                        (t           svn-status-default-log-arguments))))
    (svn-status-create-arg-file svn-status-temp-arg-file "" (svn-status-marked-files) "")
    (svn-svn-run t t 'log "log" "--targets" svn-status-temp-arg-file switches)
    (save-excursion
      (set-buffer "*svn-process*")
      (svn-log-view-mode))))

(defun svn-svn-status-rm (force)
  "Implementation of `svn-status-rm' for the SVN backend."
  (let* ((marked-files (svn-status-marked-files))
         (num-of-files (length marked-files)))
    (when (yes-or-no-p
           (if (= 1 num-of-files)
               (format "%sRemove %s? " (if force "Force " "") (svn-status-line-info->filename (car marked-files)))
             (format "%sRemove %d files? " (if force "Force " "") num-of-files)))
      (message "removing: %S" (svn-status-marked-file-names))
      (svn-status-create-arg-file svn-status-temp-arg-file "" (svn-status-marked-files) "")
      (if force
          (svn-svn-run t t 'rm "rm" "--force" "--targets" svn-status-temp-arg-file)
        (svn-svn-run t t 'rm "rm" "--targets" svn-status-temp-arg-file)))))

(defun svn-svn-status-export (src dst)
  "Implementation of `svn-status-export' for the SVN backend."
  (svn-svn-run t t 'export "export" src dst)
  (message "svn-status-export %s %s" src dest))

(defun svn-svn-status-get-specific-revision-internal (line-infos revision)
  "Implementation of `svn-status-get-specific-revision-internal' for the SVN backend."
  ;; In `svn-status-show-svn-diff-internal', there is a comment
  ;; that REVISION `nil' might mean omitting the -r option entirely.
  ;; That doesn't seem like a good idea with svn cat.
  ;;
  ;; TODO: Return the alist, instead of storing it in a variable.

  (when (eq revision :ask)
    (setq revision (svn-status-read-revision-string
                    "Get files for version: " "PREV")))

  (let ((count (length line-infos)))
    (if (= count 1)
        (let ((line-info (car line-infos)))
          (message "Getting revision %s of %s"
                   (if (eq revision :auto)
                       (if (svn-status-line-info->update-available line-info)
                           "HEAD" "BASE")
                     revision)
                   (svn-status-line-info->filename line-info)))
      ;; We could compute "Getting HEAD of 8 files and BASE of 11 files"
      ;; but that'd be more bloat than it's worth.
      (message "Getting revision %s of %d files"
               (if (eq revision :auto) "HEAD or BASE" revision)
               count)))

  (setq svn-status-get-specific-revision-file-info '())
  (dolist (line-info line-infos)
    (let* ((revision (if (eq revision :auto)
                         (if (svn-status-line-info->update-available line-info)
                             "HEAD" "BASE")
                       revision))       ;must be a string by this point
           (file-name (svn-status-line-info->filename line-info))
           ;; If REVISION is e.g. "HEAD", should we find out the actual
           ;; revision number and save "foo.~123~" rather than "foo.~HEAD~"?
           ;; OTOH, `auto-mode-alist' already ignores ".~HEAD~" suffixes,
           ;; and if users often want to know the revision numbers of such
           ;; files, they can use svn:keywords.
           (file-name-with-revision (concat file-name ".~" revision "~")))
      ;; `add-to-list' would unnecessarily check for duplicates.
      (push (cons file-name file-name-with-revision)
            svn-status-get-specific-revision-file-info)
      (save-excursion
        (let ((content
               (with-temp-buffer
                 (if (string= revision "BASE")
                     (insert-file-contents (concat (file-name-directory file-name)
                                                   (svn-svn-wc-adm-dir-name)
                                                   "/text-base/"
                                                   (file-name-nondirectory file-name)
                                                   ".svn-base"))
                   (progn
                     (svn-svn-run nil t 'cat "cat" "-r" revision file-name)
                     ;;todo: error processing
                     ;;svn: Filesystem has no item
                     ;;svn: file not found: revision `15', path `/trunk/file.txt'
                     (insert-buffer-substring "*svn-process*")))
                 (buffer-string))))
          (find-file file-name-with-revision)
          (setq buffer-read-only nil)
          (erase-buffer)  ;Widen, because we'll save the whole buffer.
          (insert content)
          (save-buffer)))))
  (setq svn-status-get-specific-revision-file-info
        (nreverse svn-status-get-specific-revision-file-info))
  (message "svn-status-get-specific-revision-file-info: %S"
           svn-status-get-specific-revision-file-info))

(defun svn-svn-status-svnversion ()
  "Implementation of `svn-status-svnversion' for the SVN backend."  
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
  "Implementation of `svn-status-base-dir' for the SVN backend."
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
