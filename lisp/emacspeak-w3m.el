;;; emacspeak-w3m.el -- speech enable emacs-w3m -- frontend for w3m WEB browser.

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

(defvar emacspeak-w3m-fontify-afer-hook nil
  "hook run after page is set up")

;;; setting keymap
(add-hook 'emacspeak-w3m-mode-hook
	  (function (lambda ()
	     (define-key w3m-mode-map "h" 'emacspeak-backward-char)
	     (define-key w3m-mode-map "l" 'emacspeak-forward-char)
)))

;;{{{ Advise top-level command

(require 'emacspeak-sounds)

(defadvice w3m-next-anchor (around emacspeak pre act)
  "Speak the anchor at distination"
  (let ((emacspeak-speak-messages nil))
    ad-do-it)
  (let ((end (next-single-property-change (point) 'w3m-href-anchor)))
    (dtk-speak (buffer-substring (point) end)))
  ad-return-value
)

(defadvice w3m-previous-anchor (around emacspeak pre act)
  "Speak the anchor at distination"
  (let ((emacspeak-speak-messages nil))
    ad-do-it)
  (let ((end (next-single-property-change (point) 'w3m-href-anchor)))
    (dtk-speak (buffer-substring (point) end)))
  ad-return-value
)

(defadvice w3m (after emacspeak pre act)
  "speak mode line"
  (dtk-speak w3m-current-title)
)

(defadvice w3m-close-window (after emacspeak pre act)
  "speak mode line"
  (emacspeak-speak-mode-line)
)

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

(defadvice w3m-view-parent-page (after emacspeak pre act)
  "speak page title"
  (dtk-speak w3m-current-title))

;;}}}

(add-hook 'w3m-mode-hook '(lambda () (run-hooks 'emacspeak-w3m-mode-hook)))

(provide 'emacspeak-w3m)

