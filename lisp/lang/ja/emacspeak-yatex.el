;;; emacspeak-yatex.el --- Speech enable Yatex -- a powerful TeX/LaTeX authoring environment
;;; $Id: emacspeak-yatex.el,v 1.1 2002/05/25 10:05:14 inoue Exp $
;;; $Author: inoue $ 
;;; DescriptionEmacspeak extensions for yatex-mode
;;; Keywords:emacspeak, audio interface to emacs YATEX
;;{{{  Copyright:

;;;Copyright (C) 2002, Bilingual Emacspeak Project
;;;  Copyright (C) 1995 -- 2002, T. V. Raman 
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;;; All Rights Reserved. 
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'voice-lock)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
;;{{{  Introduction:

;;; Provide additional advice to yatex

;;}}}
;;{{{ voice locking:

(defvar yatex-voice-lock-keywords
  (list
   '("\\(\\\\\\([a-zA-Z@]+\\|.\\)\\)" 1 voice-lock-keyword-personality t)
   '("{\\\\em\\([^}]+\\)}" 1 voice-lock-italic-personality t)
   '("{\\\\bf\\([^}]+\\)}" 1 voice-lock-bold-personality t)
   '("^[ \t\n]*\\\\def[\\\\@]\\(\\w+\\)" 1 voice-lock-function-name-personality t)
   '("\\\\\\(begin\\|end\\){\\([a-zA-Z0-9\\*]+\\)}"
     2 voice-lock-function-name-personality t)
   '("[^\\\\]\\$\\([^$]*\\)\\$" 1 voice-lock-string-personality t)
;   '("\\$\\([^$]*\\)\\$" 1 voice-lock-string-personality t)
   )
  "Additional expressions to highlight in yatex mode.")

;;{{{ hooks
(add-hook 'yatex-mode-hook
 '(lambda ()
    (setq voice-lock-keywords yatex-voice-lock-keywords)))
;;}}}

(provide 'emacspeak-yatex)

;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
