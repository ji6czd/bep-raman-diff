;;; emacspeak-navi2ch.el -- Speech enable navi2ch - 2ch Reader
;;; $Id: emacspeak-navi2ch.el,v 1.1 2003/12/23 15:51:22 inoue Exp $
;;; $Author: inoue $ 
;;; Description:  Emacspeak extension to speech enable navi2ch
;;; Keywords: Emacspeak, navi2ch, Speech, Output
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date: 2003/12/23 15:51:22 $ |
;;;  $Revision: 1.1 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 2000 -- 2002, Bilingual Emacspeak Project
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
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(eval-when (compile)
  (require 'dtk-speak)
  (require 'voice-setup)
)
(require 'emacspeak-speak)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{ variables

;;}}

;;{{{ voices
;;}}}

;;{{{ hook
;;}}}

;;{{{ Advise

(defadvice navi2ch (after emacspeak pre act)
  "Reads mode line after navi2ch startup."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-mode-line)))

(defadvice navi2ch-bm-select-article (after emacspeak pre act)
  "Reads first message after entering article mode."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
  (emacspeak-navi2ch-speak-current-message)))

(defadvice navi2ch-list-select-current-board (around emacspeak pre act)
  "Reads mode line after navi2ch startup."
  (let ((mode major-mode))
    ad-do-it
    (cond
     ((not (eq major-mode mode))
      (when (interactive-p)
	(emacspeak-auditory-icon 'select-object)
	(emacspeak-speak-mode-line))
      )
     (t
      (emacspeak-speak-line))))
  ad-return-value)

(defadvice navi2ch-exit (after emacspeak pre act)
  "Reads mode line after navi2ch exits."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-mode-line)))

(defadvice navi2ch-article-exit (after emacspeak pre act)
  "Reads mode line after article mode exits."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-mode-line)))

(defadvice navi2ch-bm-exit (after emacspeak pre act)
  "Reads mode line after board mode exits."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-mode-line)))

(defadvice navi2ch-article-next-message (after emacspeak pre act)
  "Speaks content of the message selected."
  (emacspeak-navi2ch-speak-current-message))

(defadvice navi2ch-article-previous-message (after emacspeak pre act)
  "Speaks content of the message selected."
  (emacspeak-navi2ch-speak-current-message))

(defun emacspeak-navi2ch-speak-current-message ()
  "Speaks content of the message selected."
  (interactive)
  (save-excursion
    (let* ((pt (point))
	   (beg (search-forward "\n\n" nil t))
	   (end (or (navi2ch-next-property pt 'current-number)
		    (point-max))))
      (dtk-speak (buffer-substring beg end))
      )))

;;}}}

(provide 'emacspeak-navi2ch)
