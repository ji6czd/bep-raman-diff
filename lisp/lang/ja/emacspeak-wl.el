;;; emacspeak-wl.el --- Speech enable Wanderlust -- Fluent spoken access to Yet Another Message Interface on Emacsen

;; Copyright (C) 2001 -- 2002, Bilingual Emacspeak Project

;;; $Id: emacspeak-wl.el,v 1.2 2004/11/28 09:13:40 inoue Exp $
;;; $Author: inoue $ 
;;; Description:  Emacspeak extension to speech enable wanderlust
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
(eval-when-compile
  (require 'voice-setup)
  (require 'emacspeak-keymap)
  (require 'emacspeak-fix-interactive))
(require 'emacspeak-speak)
;(require 'emacspeak-sounds)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar emacspeak-wl-unread-mark-voice 'paul-animated)
(defvar emacspeak-wl-target-mark-voice 'paul-monotone)
(defvar emacspeak-wl-important-mark-voice 'betty)


;;{{{ voice lock keywords
(defvar wl-summary-voice-lock-keywords nil
  "keywords for wl-summary-mode")
(setq wl-summary-voice-lock-keywords
      (append wl-summary-voice-lock-keywords
;	      '(("\\(^ *[0-9]+.[\\!NU]\\)" 1 emacspeak-wl-unread-mark-voice)
	      '(("\\(^ *[0-9]+.[\\!NU].*$\\)" 1 emacspeak-wl-unread-mark-voice)
		("\\(^ *[0-9]+.\\$.*$\\)" 1 emacspeak-wl-important-mark-voice)
		("\\(^ *[0-9]+\\*.*$\\)" 1 emacspeak-wl-target-mark-voice)
		)))


;;}}}

;;{{{ Change variables of Wanderlust
(setq wl-message-buffer-cache-name "*WL:Message*")
;;}}}

;;{{{ hook
(add-hook 'wl-summary-mode-hook
          (function (lambda ()
		      (make-local-variable 'voice-lock-support-mode)
		      (setq voice-lock-support-mode 'lazy-voice-lock-mode)
		      (make-local-variable 'voice-lock-defaults)
		      (setq voice-lock-defaults '(wl-summary-voice-lock-keywords t))
		      (voice-lock-mode 1)
		      (define-key wl-summary-mode-map ","    'emacspeak-wl-summary-jump-to-summary-message)
		      (define-key wl-summary-mode-map "\C-p"
			'emacspeak-wl-summary-previous-line)
		      (define-key wl-summary-mode-map "\C-n"
			'emacspeak-wl-summary-next-line)
		      (define-key wl-summary-mode-map '[up]
			'emacspeak-wl-summary-previous-line)
		      (define-key wl-summary-mode-map '[down]
			'emacspeak-wl-summary-next-line)
		      )))

;(add-hook 'wl-message-redisplay-hook
;	  (function (lambda ()
;		      (make-local-variable 'voice-lock-support-mode)
;		      (setq voice-lock-support-mode 'lazy-voice-lock-mode)
;		      (make-local-variable 'voice-lock-defaults)
;		      (setq voice-lock-defaults '(wl-message-voice-lock-keywords t))
;		      (voice-lock-mode 1))))

(add-hook 'wl-message-redisplay-hook
	  (function (lambda ()
		      (emacspeak-auditory-icon 'on))))


;;{{{ Advise top-level Wanderlust command
(defadvice wl (after emacspeak pre act )
  "read the mode line after wl starts."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice wl-exit (after emacspeak pre act )
  "read the mode line after wl End."
  (emacspeak-auditory-icon 'close-object)
  (emacspeak-speak-mode-line))

(defadvice wl-summary-exit (after emacspeak pre act)
  "read the mode line after wl Summary End."
  (set-buffer "Folder")
  (emacspeak-auditory-icon 'close-object)
  (emacspeak-speak-mode-line))

(defadvice wl-folder-prev-entity (after emacspeak pre act)
  "speeks the message after movement for folder mode"
  (emacspeak-speak-line))

(defadvice wl-folder-next-entity (after emacspeak pre act)
  "speeks the message after movement for folder mode"
  (emacspeak-speak-line))

(defadvice wl-folder-prev-unread (after emacspeak pre act)
  "speeks the message after movement for folder mode"
  (emacspeak-speak-line))

(defadvice wl-folder-next-unread (after emacspeak pre act)
  "speeks the message after movement for folder mode"
  (emacspeak-speak-line))

(defadvice wl-folder-goto-first-unread-folder (after emacspeak pre act)
  "speak the message unread folder for summary"
  (emacspeak-speak-line))

(defadvice wl-folder-suspend (after emacspeak pre act)
  "announces after wanderlust suspends."
  (emacspeak-auditory-icon 'close-object)
  (emacspeak-speak-mode-line))

(defadvice wl-summary-prev (after emacspeak pre act)
  "speeks the message after movement"
;  (emacspeak-speak-line)
;(emacspeak-wl-read-temp-mark)
;(emacspeak-wl-read-permanent-mark)
(let ((dtk-stop-immediately nil))
(emacspeak-wl-summary-speak-current-line-with-markmeaning)
)
  )

(defadvice wl-summary-next (after emacspeak pre act)
  "speeks the message after movement"
;  (emacspeak-speak-line)
(let ((dtk-stop-immediately nil))
(emacspeak-wl-summary-speak-current-line-with-markmeaning)
)
  )

(defadvice wl-summary-display-top (after emacspeak pre act)
  "speeks the message after movement"
;  (emacspeak-speak-line)
(let ((dtk-stop-immediately nil))
(emacspeak-wl-summary-speak-current-line-with-markmeaning)
)
  )

(defadvice wl-summary-display-bottom (after emacspeak pre act)
  "speeks the message after movement"
;  (emacspeak-speak-line)
(let ((dtk-stop-immediately nil))
(emacspeak-wl-summary-speak-current-line-with-markmeaning)
)
  )

		    
(defadvice wl-summary-goto-folder (after emacspeak pre act)
  "speeks the message after folder movement"
  (emacspeak-speak-line)
  )

(defadvice wl-summary-reply (after emacspeak pre act )
  "speeks the current line after new message is opened."
  (dtk-set-punctuations-to-all)
  (dtk-speak "draft is prepared")
  )

(defadvice wl-summary-reply-with-citation (after emacspeak pre act )
  "Tells that draft is prepared"
  (dtk-set-punctuations-to-all)
  (dtk-speak "draft with citation is prepared")
  )

(defadvice wl-summary-write (after emacspeak pre act )
  "Tells that draft is prepared"
  (dtk-set-punctuations-to-all)
  (dtk-speak "draft is prepared")
  )

(defadvice wl-summary-forward (after emacspeak pre act )
  "Tells that draft is prepared"
  (dtk-set-punctuations-to-all)
  (dtk-speak "draft is prepared")
  )

(defadvice wl-summary-reedit(after emacspeak pre act)
  "Tells that draft is prepared"
  (dtk-set-punctuations-to-all)
  (dtk-speak "draft is prepared")
  )

(defadvice wl-summary-resend-bounced-mail (after emacspeak pre act)
  "Tells that draft is prepared"
  (dtk-set-punctuations-to-all)
  (dtk-speak "draft is prepared")
  )

(defadvice wl-summary-write-current-folder (after emacspeak pre act)
  "Tells that draft is prepared"
  (dtk-set-punctuations-to-all)
  (dtk-speak "draft is prepared")
  )
 
(defadvice wl-draft (after emacspeak pre act )
  "Tells that draft is prepared"
  (dtk-set-punctuations-to-all)
  (dtk-speak "draft is prepared")
  )

(defadvice wl-draft-beginning-of-line (before emacspeak pre act)
  "Stop speech first."
  (when (interactive-p) (dtk-stop )
        (emacspeak-auditory-icon 'select-object)))

(defadvice wl-draft-yank-original (after emacspeak pre act)
  "provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object)))

(defadvice wl-summary-down (after emacspeak pre act)
  "speeks the message after movement"
;  (emacspeak-speak-line)
(let ((dtk-stop-immediately nil))
(emacspeak-wl-summary-speak-current-line-with-markmeaning)
)
  )

(defadvice wl-summary-up (after emacspeak pre act)
  "speeks the message after movement"
;  (emacspeak-speak-line)
(let ((dtk-stop-immediately nil))
(emacspeak-wl-summary-speak-current-line-with-markmeaning)
)
  )

;(defadvice wl-summary-exit (after emacspeak pre act)
;  "speeks the message aftersummary quit  "
;  (emacspeak-speak-mode-line)
;)

;(defadvice wl-summary-force-exit (after emacspeak pre act)
;  "speeks the message aftersummary quit  "
;  (emacspeak-speak-mode-line)
;)

(defadvice wl-folder-jump-to-current-entity (after emacspeak pre act)
  "Speak The Message after folder jumped current "
  (emacspeak-speak-mode-line)
  )  

(defadvice wl-summary-jump-to-current-message (after emacspeak pre act)
  "speak msg number and go to end of the header"
  (dtk-speak wl-message-buffer-cur-number)
  (search-forward "\n\n")
  (backward-char))

(defadvice wl-summary-jump-to-parent-message (after emacspeak pre act)
  "speak the parent message"
  (emacspeak-speak-line))

(defadvice wl-draft-mimic-kill-buffer (after emacspeak pre act)
  "anounce kill draft buffer"
  (emacspeak-auditory-icon 'close-object)
  (emacspeak-speak-mode-line))

(defadvice insert-signature (after emacspeak pre act)
  "announce insert signature."
  (dtk-speak "inserted signature"))

(defadvice wl-template-select (after emacspeak pre act)
  "provide auditory feedback"
  (emacspeak-auditory-icon 'open-object))

(defadvice wl-template-set (after emacspeak pre act)
  "provide auditory feedback"
  (dtk-speak "template set")
(emacspeak-auditory-icon 'select-object))

(defun emacspeak-wl-summary-jump-to-summary-message ()
  "Jump to message which cursor located on summary buffer and speak msg number, go to end of the header"
  (interactive)
  (if (wl-summary-message-number)
      (progn (wl-summary-redisplay)
	     (wl-summary-jump-to-current-message))
    (message "No message to display.")))


;;}}}
(provide 'emacspeak-wl)

(defun emacspeak-wl-read-temp-mark ()
(interactive)
(setq msg-no (wl-summary-message-number))
(if 
(wl-summary-msg-marked-as-target msg-no)
(setq temp-mark "target")
(if (wl-summary-msg-marked-as-copied msg-no)
(setq temp-mark "copy")
(if (wl-summary-msg-marked-as-deleted msg-no)
(setq temp-mark "delete")
(if (wl-summary-msg-marked-as-refiled msg-no)
(setq temp-mark "refile")
(setq temp-mark " ")
))))
temp-mark
)

(defun emacspeak-wl-read-permanent-mark ()
  (interactive)
  (beginning-of-line)
(when (looking-at "^ *\\([0-9]+\\)[^0-9]\\([^0-9]\\)")
  (progn
    (setq mark (wl-match-buffer 2))
    (if (string= mark wl-summary-new-mark)
	(setq per-mark "new")
      (if (string=  mark wl-summary-unread-uncached-mark)
(setq per-mark "unread uncached")
      (if (string= mark wl-summary-important-mark)
(setq per-mark "important")
(if (string= mark wl-summary-unread-cached-mark)
(setq per-mark "unread")
(if (string= mark wl-summary-read-uncached-mark)
(setq per-mark "uncached")
(setq per-mark " ")
)))))))
per-mark)


(defun emacspeak-wl-summary-speak-current-line-with-markmeaning ()
(interactive)
(save-excursion
(beginning-of-line)
(setq start (point))
(end-of-line)
(setq end (point))
(setq line (buffer-substring start end ))
(let ((dtk-stop-immediately t))
;Next line is to speak summary current message with mark meaning.
;(dtk-speak (concat (emacspeak-wl-read-temp-mark)  (emacspeak-wl-read-permanent-mark) line))
(dtk-speak line)
)))



(defun emacspeak-wl-summary-speak-line ()
  "Speaks current summary line as specified."
(emacspeak-wl-summary-speak-current-line-with-markmeaning)
)

(defun emacspeak-wl-summary-next-line (arg)
  (interactive "p")
  (next-line arg)
  (emacspeak-wl-summary-speak-line)
)

(defun emacspeak-wl-summary-previous-line (arg)
  (interactive "p")
  (previous-line arg)
  (emacspeak-wl-summary-speak-line)
)
