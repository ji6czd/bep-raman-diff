;;; emacspeak-ja-setup.el --- Setup emacspeak with Japanese extension

;;; $Id: emacspeak-ja-setup.el,v 1.2 2004/11/28 09:13:39 inoue Exp $
;;; $Author: inoue $
;;; Description:  Module to set up dtk voices and personalities
;;; Keywords: Voice, Personality, BEP
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2004/11/28 09:13:39 $ |
;;;  $Revision: 1.2 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 2001 -- 2002, Bilingual Emacspeak Project
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
;;; Commentary:

;;; This file is present for convenience of for BEP users.
;;; This file call emacspeak-setup first, multilingual extension,
;;; Japanese-specific extension, and setup some application settings.
;;; You can load Japanese capable Emacspeak by simply loading this file.
;; 

;;; Code:

;;{{ Require

(eval-when-compile
  (require 'cl))
;;}}

(defvar emacspeak-ja-setup-hook nil
  "hook variable called after the setup of BEP finished.")

(defcustom bep-default-speech-rate 225
  "Default speech rate for bep-ss."
  :group 'tts
  :type 'integer)


(defun emacspeak-ja()

  ;;; Turn off unibyte mode
  (setq emacspeak-unibyte nil)
  ;;; Unspeekable rule
  (setq emacspeak-unspeakable-rule   "^\\W+$")

;;; Set up Emacspeak first.
  (load
   (expand-file-name "emacspeak-setup"
		     (concat (file-name-directory load-file-name) "../..")))

;;; Load Multilingual extension.
  (setq emacspeak-m17n-auto-put-language-mode t)
  (require 'emacspeak-m17n "lang/emacspeak-m17n")

;; re-initialize speech server
  (tts-configure-synthesis-setup dtk-program)

;; Make some interactive commands speak
  (emacspeak-fix-interactive 'emacspeak-m17n-set-rate-offset)

;;; Japanese specific settings
  (require 'emacspeak-ja-tbl)
  (emacspeak-m17n-register-display-table 'ja 'auditory-disp-table-ja)
  (require 'emacspeak-m17n-ja)
  (emacspeak-m17n-add-put-language-strategy 'emacspeak-m17n-put-language-ja-ne)
  (emacspeak-m17n-add-put-language-strategy 'emacspeak-m17n-put-language-ja-ke-1)
  (emacspeak-m17n-add-put-language-strategy 'emacspeak-m17n-put-language-ja-ke-all)
  (if (not emacspeak-m17n-put-language-strategy)
      (setq-default emacspeak-m17n-put-language-strategy
	    'emacspeak-m17n-put-language-ja-ke-all))
  (if (not emacspeak-m17n-put-language-internal-strategy)
      (setq emacspeak-m17n-put-language-internal-strategy
	    'emacspeak-m17n-put-language-ja-ke-all))
  (when emacspeak-m17n-auto-put-language-mode
    (emacspeak-m17n-put-language-install))

;;; Do additional package setup.
  (emacspeak-do-package-setup "egg" 'emacspeak-egg)
  (emacspeak-do-package-setup "quail" 'emacspeak-kkc)
  (emacspeak-do-package-setup "liece" 'emacspeak-liece)
  (emacspeak-do-package-setup "mew" 'emacspeak-mew)
  (emacspeak-do-package-setup "mime-setup" 'emacspeak-semi)
  (emacspeak-do-package-setup "navi2ch" 'emacspeak-navi2ch)
  (emacspeak-do-package-setup "w3m" 'emacspeak-w3m)
  (emacspeak-do-package-setup "wl" 'emacspeak-wl)
  (emacspeak-do-package-setup "yahtml" 'yahtml-voice)
  (emacspeak-do-package-setup "yatex" 'emacspeak-yatex)

  ;; Add hooks
  (add-hook 'yahtml-mode-hook 'emacspeak-setup-programming-mode)
  (add-hook 'yatex-mode-hook 'emacspeak-setup-programming-mode)

;;; Define keys
  (define-key 'emacspeak-personal-keymap "ms" 'emacspeak-m17n-ja-change-strategy)
  (define-key 'emacspeak-personal-keymap "ma" 'emacspeak-m17n-auto-put-language-mode)
  (define-key 'emacspeak-personal-keymap "mr" 'emacspeak-m17n-set-rate-offset)

  (run-hooks 'emacspeak-ja-setup-hook)
)

(emacspeak-ja)

(provide 'emacspeak-ja-setup)
;;; bep-setup.el ends here
