;;; emacspeak-w3m.el -- speech enable emacs-w3m -- frontend for w3m WEB browser.
;;;$Id: emacspeak-w3m.el,v 1.6 2002/02/07 11:54:10 mitsugu Exp $
;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2000, T. V. Raman 
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

;;; code:
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(eval-when (compile)
  (require 'w3m)
  (require 'voice-lock)
  (require 'emacspeak-keymap)
  (require 'emacspeak-fix-interactive))
(require 'emacspeak-speak)

;;; hooks
(defvar emacspeak-w3m-mode-hook nil
  "hook run after entering w3m-mode")

(defvar emacspeak-w3m-fontify-after-hook nil
  "hook run after page is set up")

(defvar emacspeak-w3m-href-personality 'betty
  "Personality to speak hyperlinks.")

;;; setting keymap & default settings
(add-hook 'emacspeak-w3m-mode-hook
	  (function (lambda ()
	     (define-key w3m-mode-map "h" 'emacspeak-backward-char)
	     (define-key w3m-mode-map "l" 'emacspeak-forward-char)
	     (setq w3m-after-cursor-move-hook   '(w3m-highlight-current-anchor))
)))

(add-hook 'emacspeak-w3m-fontify-after-hook
	  'emacspeak-w3m-voicify)

;;{{{ Advise top-level command

(require 'emacspeak-sounds)

(defadvice w3m-next-anchor (around emacspeak pre act)
  "Speak the anchor at distination"
  (let ((dtk-stop-immediately t)
	(emacspeak-speak-messages nil))
    ad-do-it)
  (let ((end (next-single-property-change (point) 'w3m-href-anchor)))
    (dtk-speak (buffer-substring (point) end)))
  ad-return-value
)

(defadvice w3m-previous-anchor (around emacspeak pre act)
  "Speak the anchor at distination"
  (let ((dtk-stop-immediately t)
	(emacspeak-speak-messages nil))
    ad-do-it)
  (let ((end (next-single-property-change (point) 'w3m-href-anchor)))
    (dtk-speak (buffer-substring (point) end)))
  ad-return-value
)

(defadvice w3m (after emacspeak pre act)
  "speak page title"
  (dtk-speak w3m-current-title))

(defadvice w3m-find-file (after emacspeak pre act)
  "speak page title"
  (dtk-speak w3m-current-title)
)

(defadvice w3m-close-window (after emacspeak pre act)
  "speak mode line"
  (emacspeak-speak-mode-line)
)

(defadvice w3m-quit (after emacspeak pre act)
  "speak the modeline"
  (emacspeak-speak-mode-line))

(defadvice w3m-view-this-url (after emacspeak pre act)
  "speak page title"
  (dtk-speak w3m-current-title))

(defadvice w3m-reload-this-page (after emacspeak pre act)
  "speak page title"
  (dtk-speak w3m-current-title))

(defadvice w3m-view-previous-page (after emacspeak pre act)
  "speak page title"
  (dtk-speak w3m-current-title))

(defadvice w3m-view-next-page (after emacspeak pre act)
  "speak page title"
  (dtk-speak w3m-current-title))
(defadvice w3m-edit-current-url (after emacspeak pre act)
  "enter edit page speak the mode line"
(emacspeak-speak-mode-line))

(defadvice w3m-view-parent-page (after emacspeak pre act)
  "speak page title"
  (dtk-speak w3m-current-title))
(defadvice w3m-edit-current-url (after emacspeak pre act)
  "edit current URL after speak the modeline"
  (emacspeak-speak-mode-line))

(defun emacspeak-w3m-voicify ()
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (let ((beg (or (and (get-text-property (point-min) 'w3m-href-anchor)
			  (point-min))
		     (next-single-property-change (point-min)
						  'w3m-href-anchor
						  nil (point-max)))))
	(while (< beg (point-max))
	  (setq end (next-single-property-change beg 'w3m-href-anchor
						 nil (point-max)))
	  (message (format "%s %s" beg end))
	  (when (get-text-property beg 'w3m-href-anchor)
	    (put-text-property beg end
			       'personality emacspeak-w3m-href-personality))
	  (setq beg end))))))

;;}}}

(add-hook 'w3m-mode-hook '(lambda () (run-hooks 'emacspeak-w3m-mode-hook)))
(add-hook 'w3m-fontify-after-hook
	  '(lambda () (run-hooks 'emacspeak-w3m-fontify-after-hook)))

(provide 'emacspeak-w3m)

