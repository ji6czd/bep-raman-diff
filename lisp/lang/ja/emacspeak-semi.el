;;; emacspeak-semi.el --- Speech enable semi -- Fluent spoken access to MIME User Interface on emacsen

;; Copyright (C) 2001 -- 2002, Bilingual Emacspeak Project

;;; $Id: emacspeak-semi.el,v 1.2 2004/11/28 09:13:40 inoue Exp $
;;; $Author: inoue $ 
;;; Description:  Emacspeak extension to speech enable semi
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
(require 'dtk-speak)
(require 'emacspeak-speak)
(require 'voice-setup)
;(require 'emacspeak-sounds)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ Advise top-level semi command
;;; for mime-view
(defadvice mime-preview-move-to-previous (after emacspeak pre act)
  "speak the previous line for mime-preview"
  (emacspeak-speak-line))

(defadvice mime-preview-move-to-next (after emacspeak pre act)
  "speak the next line of mime-preview"
  (emacspeak-speak-line))

(defadvice mime-preview-scroll-up-entity (after emacspeak pre act)
  "speak the scrool up mime entity"
  (emacspeak-speak-line))

(defadvice mime-preview-scroll-down-entity (after emacspeak pre act)
  "speak the scrool down mime entity"
  (emacspeak-speak-line))

(defadvice mime-preview-next-line-entity (after emacspeak pre act)
  "speak the next-line-entity for mime-view"
  (emacspeak-speak-line))

(defadvice mime-preview-next-line-entity (after emacspeak pre act)
  "speak the previous-line-entity for mime-view"
  (emacspeak-speak-line))

(defadvice mime-preview-quit (after emacspeak pre act)
  "announce the mime-view quit"
  (emacspeak-speak-mode-line))
(defadvice mime-edit-insert-message (after emacspeak pre act)
  "announce the message insert draft"
  (emacspeak-speak-line))
(defadvice mime-edit-insert-signature (after emacspeak pre act)
  "announce insert signature."
  (dtk-speak "inserted signature"))
(defadvice mime-edit-enclose-alternative-region (after emacspeak pre act)
  " speak enclose as multipart alternative region"
(emacspeak-speak-line))
(defadvice mime-edit-enclose-parallel-region (after emacspeak pre act)
  "speak enclose multipart parallel region"
  (emacspeak-speak-line))
(defadvice mime-edit-enclose-mixed-region (after emacspeak pre act)
  "speak enclose multipart mixed region"
  (emacspeak-speak-line))
(defadvice mime-edit-enclose-digest-region (after emacspeak pre act)
  "speak enclose multipart digest region"
  (emacspeak-speak-line))
(defadvice mime-edit-enclose-pgp-signed-region (after emacspeak pre act)
  "speak enclose pgp-sign region"
  (emacspeak-speak-line))
(defadvice mime-edit-enclose-pgp-encrypted-region (after emacspeak pre act)
  "speak enclose pgp-encrypted region"
  (emacspeak-speak-line))
(defadvice mime-edit-preview-message (after emacspeak pre act)
  "speak the preview mode line"
  (emacspeak-speak-mode-line))

;;}}}
;;{{{ voice-lock
;;{{{ voice lock keywords
(defvar emacspeak-mime-view-cite-voice  'paul-monotone)

(defvar mime-view-voice-lock-keywords nil
  "keywords for mime-view-mode")

(setq mime-view-voice-lock-keywords
      (append mime-view-voice-lock-keywords
              '(
                ("^\\(\\([ \t]*\\w*[A-Za-z0-9'-]*[>|]+\\)+\\).*" 0 emacspeak-mime-view-cite-voice )
)))

(add-hook 'mime-view-mode-hook
	  (function (lambda ()
		      (make-local-variable 'voice-lock-support-mode)
		      (setq voice-lock-support-mode 'lazy-voice-lock-mode)
		      (make-local-variable 'voice-lock-defaults)
		      (setq voice-lock-defaults '(mime-view-voice-lock-keywords t))
		      (voice-lock-mode 1)
)))
;;}}}

(provide 'emacspeak-semi)

