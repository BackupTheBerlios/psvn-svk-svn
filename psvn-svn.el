;;; psvn-svn.el --- Subversion support for psvn.el / Emacs
;; Copyright (C) 2002-2005 by Stefan Reichoer & intrigeri

;; Authors:
;;   - Stefan Reichoer, <stefan@xsteve.at>
;;   - Ben Voui, <intrigeri@boum.org>
;; $Id$
;; $URL$

;; Contains some code from VC, under GPL:
;;   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1998, 1999, 2000, 2002,
;;   2003, 2004, 2005 Free Software Foundation, Inc.

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

(provide 'psvn-svn)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; psvn-svn.el ends here
