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
  (if (not
       (string-match
	"^Checkout Path:"
	(shell-command-to-string (concat "svk info " (expand-file-name dir)))))
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
              svk-status-remote (when arg t))
        (svn-svk-run t t 'status "status" status-option)))))


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
