;;; emacspeak-m17n-load-path.el -- Setup Emacs load-path for compiling Emacspeak

;; Copyright (C) 2001 -- 2002, Bilingual Emacspeak Project

;; Author: Koichi INOUE <inoue@argv.org>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Commentary
;; Add this directory and subdirectories to load-path
(let ((here (file-name-directory load-file-name))
      file-list dir)
  (load (concat here "../emacspeak-load-path"))
  (unless (member here load-path)
      (setq load-path
	    (cons here load-path )))
  ;;; Add subdirectories to load-path
  (setq file-list (directory-files here))
  (while file-list
    (setq dir (car file-list))
    (if (and
	 (file-directory-p (concat here "/" dir))
	 (zerop (string-match "\\(^CVS$\\|^RCS$\\|\.\\|\.\.\\)" dir)))
	(progn
	  (setq load-path
		(cons (concat here "/" dir) load-path))
	  ))
      (setq file-list (cdr file-list))
))

(provide 'emacspeak-load-path)
