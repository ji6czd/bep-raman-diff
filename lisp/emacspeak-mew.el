;;; emacspeak-mew.el --- Speech enable Mew -- Fluent spoken access to internet message
;;; $Id: emacspeak-mew.el,v 1.4 2001/06/21 08:23:29 mitsugu Exp $
;;; $Author: mitsugu $ 
;;; Description:  Emacspeak extension to speech enable mew
;;; Keywords: Emacspeak, Mew, IM, mail, Advice, Spoken Output
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date: 2001/06/21 08:23:29 $ |
;;;  $Revision: 1.4 $ | 
;;; Location undetermined
;;;

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

;;}}}
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(eval-when (compile)
  (require 'mew)
  (require 'dtk-voices)
  (require 'mew-vars2)
  (require 'mew-summary)
  (require 'mew-message)
  (require 'mew-virtual)
  (require 'mew-mark)
)
(require 'emacspeak-speak)

(require 'emacspeak-sounds)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ voices
(defvar emacspeak-mew-mark-delete-voice 'paul-monotone)
(defvar emacspeak-mew-mark-review-voice 'paul-animated)
(defvar emacspeak-mew-mark-refile-voice 'betty)
(defvar emacspeak-mew-mark-multi-voice 'paul-animated)
(defvar emacspeak-mew-cite-voice  'paul-monotone)

;;}}}

;;{{{ voice lock keywords
(defvar mew-summary-voice-lock-keywords nil
  "keywords for mew-summary-mode")
(setq mew-summary-voice-lock-keywords
      (append mew-summary-voice-lock-keywords
	      '(("\\(^ *[0-9]+D\\)" 1 emacspeak-mew-mark-delete-voice)
		("\\(^ *[0-9]+o\\)" 1 emacspeak-mew-mark-refile-voice)
		("\\(^ *[0-9]+\\*\\)" 1 emacspeak-mew-mark-review-voice)
		("\\(^ *[0-9]+@\\)" 1 emacspeak-mew-mark-multi-voice)
		)))
(defvar mew-message-voice-lock-keywords nil
  "keywords for mew-message-mode")
(setq mew-message-voice-lock-keywords
      (append mew-message-voice-lock-keywords
              '(
                ("^\\(\\([ \t]*\\w*[A-Za-z0-9'-]*[>|]+\\)+\\).*" 0 emacspeak-mew-cite-voice )
)))

(defvar mew-virtual-voice-lock-keywords nil
  "keywords for mew-virtual-mode")
(setq mew-virtual-voice-lock-keywords
      (append mew-virtual-voice-lock-keywords
	      '(("\\(^ *[0-9]+D\\)" 1 emacspeak-mew-mark-delete-voice)
		("\\(^ *[0-9]+o\\)" 1 emacspeak-mew-mark-refile-voice)
		("\\(^ *[0-9]+\\*\\)" 1 emacspeak-mew-mark-review-voice)
		("\\(^ *[0-9]+@\\)" 1 emacspeak-mew-mark-multi-voice)
		)))

;;}}}

;;{{{ hook
(add-hook 'mew-summary-mode-hook
	  (function (lambda ()
		      (make-local-variable 'voice-lock-support-mode)
		      (setq voice-lock-support-mode 'lazy-voice-lock-mode)
		      (make-local-variable 'voice-lock-defaults)
		      (setq voice-lock-defaults '(mew-summary-voice-lock-keywords t))
		      (voice-lock-mode 1)

		      (define-key mew-summary-mode-map "\C-p"
			'emacspeak-mew-summary-previous-line)
		      (define-key mew-summary-mode-map "\C-n"
			'emacspeak-mew-summary-next-line)
		      (define-key mew-summary-mode-map '[up]
			'emacspeak-mew-summary-previous-line)
		      (define-key mew-summary-mode-map '[down]
			'emacspeak-mew-summary-next-line)
		      )))

(add-hook 'mew-message-mode-hook
	  (function (lambda ()
		      (make-local-variable 'voice-lock-support-mode)
		      (setq voice-lock-support-mode 'lazy-voice-lock-mode)
		      (make-local-variable 'voice-lock-defaults)
		      (setq voice-lock-defaults '(mew-message-voice-lock-keywords t))
		      (voice-lock-mode 1)
		      )))

(add-hook 'mew-virtual-mode-hook
	  (function (lambda ()
		      (make-local-variable 'voice-lock-support-mode)
		      (setq voice-lock-support-mode 'lazy-voice-lock-mode)
		      (make-local-variable 'voice-lock-defaults)
		      (setq voice-lock-defaults '(mew-virtual-voice-lock-keywords t))
		      (voice-lock-mode 1)
		      )))
;;}}}

;;{{{ Advise top-level Mew command
(defadvice mew (after emacspeak pre act )
  "read the mode line after Mew starts."
  (emacspeak-speak-mode-line))

(defadvice mew-summary-suspend (after emacspeak pre act )
  "announces after Mew suspends."
;  (dtk-interp-queue "Mew suspended")
  (emacspeak-speak-mode-line))

(defadvice mew-summary-quit (after emacspeak pre act )
  "announces after Mew quitss."
;  (dtk-interp-queue "Mew quit")
  (emacspeak-speak-mode-line))

(defadvice mew-kill-buffer (after emacspeak pre act)
  "announce kill buffer"
  (emacspeak-speak-mode-line))

(defadvice mew-draft-kill (after emacspeak pre act)
  "announce kill draft buffer"
  (emacspeak-speak-mode-line))

(defadvice mew-draft-circular-comp (around emacspeak pre act)
  "Say what you completed."
  (let* ((beg (emacspeak-mew-get-value))
        (dtk-stop-immediately t)
	(prior-key (buffer-substring beg (point)))
	(prior (point)))
    (emacspeak-kill-buffer-carefully "*Mew completions*")
    ad-do-it
    (let ((completions-buffer (get-buffer "*Mew completions*")))
      (if (or (> (point) prior)
	      (not (string-equal prior-key (buffer-substring beg prior))))
          (dtk-speak (buffer-substring beg (point )))
        (when (and completions-buffer
                   (window-live-p (get-buffer-window completions-buffer )))
          (save-excursion
            (set-buffer completions-buffer )
            (emacspeak-prepare-completions-buffer)
            (dtk-speak (buffer-string ))))))
    ad-return-value))

(defadvice mew-complete-field (around emacspeak pre act)
  "Say what you completed."
  (let ((prior (point ))
        (dtk-stop-immediately t))
    (emacspeak-kill-buffer-carefully "*Mew completions*")
    ad-do-it
    (let ((completions-buffer (get-buffer "*Mew completions*")))
      (if (> (point) prior)
          (dtk-speak (buffer-substring prior (point )))
        (when (and completions-buffer
                   (window-live-p (get-buffer-window completions-buffer )))
          (save-excursion
            (set-buffer completions-buffer )
            (emacspeak-prepare-completions-buffer)
            (dtk-speak (buffer-string ))))))
    ad-return-value))

(defadvice mew-complete-address (around emacspeak pre act)
  "Say what you completed."
  (let* ((beg (emacspeak-mew-backward-char))
	 (dtk-stop-immediately t)
	 (prior-key (buffer-substring beg (point)))
	 (prior (point)))
    (emacspeak-kill-buffer-carefully "*Mew completions*")
    ad-do-it
    (let ((completions-buffer (get-buffer "*Mew completions*")))
      (if (and (> (point) prior)
	       (string-equal prior-key (buffer-substring beg prior)))
          (dtk-speak (buffer-substring prior (point )))
	(if (not (string-equal prior-key (buffer-substring beg prior)))
	    (dtk-speak (concat "changed \n" 
			       (buffer-substring beg (point))))
	  (when (and completions-buffer
		     (window-live-p (get-buffer-window completions-buffer )))
	    (save-excursion
	      (set-buffer completions-buffer )
	      (emacspeak-prepare-completions-buffer)
	      (dtk-speak (buffer-string )))))))
    ad-return-value))

(defadvice mew-complete-folder (around emacspeak pre act)
  "Say what you completed."
  (let ((prior (point ))
        (dtk-stop-immediately t))
    (emacspeak-kill-buffer-carefully "*Mew completions*")
    ad-do-it
    (let ((completions-buffer (get-buffer "*Mew completions*")))
      (if (> (point) prior)
          (dtk-speak (buffer-substring prior (point )))
        (when (and completions-buffer
                   (window-live-p (get-buffer-window completions-buffer )))
          (save-excursion
            (set-buffer completions-buffer )
            (emacspeak-prepare-completions-buffer)
            (dtk-speak (buffer-string ))))))
    ad-return-value))

(defadvice mew-complete-newsgroups (around emacspeak pre act)
  "Say what you completed."
  (let ((prior (point ))
        (dtk-stop-immediately t))
    (emacspeak-kill-buffer-carefully "*Mew completions*")
    ad-do-it
    (let ((completions-buffer (get-buffer "*Mew completions*")))
      (if (> (point) prior)
          (dtk-speak (buffer-substring prior (point )))
        (when (and completions-buffer
                   (window-live-p (get-buffer-window completions-buffer )))
          (save-excursion
            (set-buffer completions-buffer )
            (emacspeak-prepare-completions-buffer)
            (dtk-speak (buffer-string ))))))
    ad-return-value))

(defadvice mew-complete-config (around emacspeak pre act)
  "Say what you completed."
  (let ((prior (point ))
        (dtk-stop-immediately t))
    (emacspeak-kill-buffer-carefully "*Mew completions*")
    ad-do-it
    (let ((completions-buffer (get-buffer "*Mew completions*")))
      (if (> (point) prior)
          (dtk-speak (buffer-substring prior (point )))
        (when (and completions-buffer
                   (window-live-p (get-buffer-window completions-buffer )))
          (save-excursion
            (set-buffer completions-buffer )
            (emacspeak-prepare-completions-buffer)
            (dtk-speak (buffer-string ))))))
    ad-return-value))

(defadvice mew-summary-display-up (after emacspeak pre act )
  "speeks the message after movement"
  (emacspeak-mew-summary-speak-line)
)

(defadvice mew-summary-display-down (after emacspeak pre act )
  "speeks the message after movement"
  (emacspeak-mew-summary-speak-line)
)

(defadvice mew-summary-jump-top (after emacspeak pre act )
  "speeks the message after movement"
  (emacspeak-speak-line)
)

(defadvice mew-summary-jump-bottom (after emacspeak pre act )
  "speeks the message after movement"
  (emacspeak-speak-line)
)

(defadvice mew-summary-jump-message (after emacspeak pre act )
  "speeks the message after movement"
  (emacspeak-speak-line)
)

(defadvice mew-summary-display-review-up (after emacspeak pre act )
  "speeks the message after movement"
  (emacspeak-speak-line)
)

(defadvice mew-summary-display-review-down (after emacspeak pre act )
  "speeks the message after movement"
  (emacspeak-speak-line)
)

(defadvice mew-summary-goto-folder (after emacspeak pre act )
  "speeks the message after movement"
  (emacspeak-speak-line)
)

(defadvice mew-summary-delete (after emacspeak pre act )
  "tells the message is marked for delete."
  (dtk-interp-queue "delete")
  (emacspeak-speak-line)
)

(defadvice mew-summary-refile (after emacspeak pre act )
  "tells the message is marked for refile."
  (emacspeak-speak-line)
)

(defadvice mew-summary-undo (around emacspeak pre act )
  "tells the message is unmarked."
  (let ((mark (mew-summary-get-mark)))
    ad-do-it
    (if (and mark (not (mew-summary-get-mark))) (dtk-speak "mark canceled"))
))

(defadvice mew-summary-multi (after emacspeak pre act )
  "tells the message is marked for multi."
  (dtk-speak "multi")
)

(defadvice mew-summary-review (after emacspeak pre act )
  "tells the message is marked for review."
  (dtk-speak "review")
)

(defadvice mew-summary-mark-all (after emacspeak pre act )
  "tells that all messages are marked"
  (dtk-speak "marked all messages")
)

(defadvice mew-summary-mark-delete (after emacspeak pre act )
  "tells that all messages are marked for delete"
  (dtk-speak "marked all messages for delete")
)


(defadvice mew-message-next-msg (after emacspeak pre act )
  "speaks the summary line or message number"
  (save-excursion
    (let ((sum (mew-current-get-fld (mew-frame-id)))
	  (num (mew-current-get-msg (mew-frame-id))))
      (if (not (get-buffer sum))
	  (dtk-speak (format "%s of %s" num sum))
	(progn
;	  (set-buffer sum)
;	  (emacspeak-speak-line)
	  (dtk-speak (format "%s of %s" num sum))
	  ))
)))

(defadvice mew-summary-send (after emacspeak pre act )
  "speeks the current line after new message is opened."
  (dtk-set-punctuations-to-all)
  (dtk-speak "draft is prepared")
)

(defadvice mew-summary-send-to-others (after emacspeak pre act )
  "speeks the current line after new message is opened."
  (dtk-set-punctuations-to-all)
  (dtk-speak "draft is prepared")
)

(defadvice mew-summary-reply (after emacspeak pre act )
  "tells that draft is prepared"
  (dtk-set-punctuations-to-all)
  (dtk-speak "draft is prepared")
)

(defadvice mew-summary-reply-with-citation (after emacspeak pre act )
  "Tells that draft is prepared"
  (dtk-set-punctuations-to-all)
  (dtk-speak "draft with citation is prepared")
)

(defadvice mew-summary-addrbook-add (after emacspeak pre act )
  "Tells that addrbook buffer is prepared"
  (dtk-set-punctuations-to-all)
  (dtk-speak "addrbook buffer is prepared")
)

(defadvice mew-send (after emacspeak pre act )
  "Tells that draft is prepared"
  (dtk-set-punctuations-to-all)
  (dtk-speak "draft is prepared")
)

(defadvice mew-draft-insert-signature (after emacspeak pre act)
  "announce insert signature."
  (dtk-speak "inserted signature"))

(defadvice mew-draft-prepare-attachments (after emacspeak pre act )
  "speaks the current line."
  (if (not ad-return-value)
  (emacspeak-speak-line)
))

(defadvice mew-attach-next (after emacspeak pre act )
  "speaks the current line."
  (emacspeak-speak-line)
)

(defadvice mew-attach-previous (after emacspeak pre act )
  "speaks the current line."
  (emacspeak-speak-line)
)

(defadvice mew-summary-make-thread (around emacspeak pre act)
  "announce when Making thread"
  (dtk-speak "making thread...")
  (let ((emacspeak-speak-messages nil))
    ad-do-it)
  (emacspeak-speak-mode-line)
  ad-return-value
)

;;; helper functions
(defun emacspeak-mew-speak-mark (&optional queue-only)
  "speaks the mark"
  (interactive)
  (let ((mark (mew-summary-get-mark)))
    (cond
     ((eq mark mew-mark-multi) (dtk-speak "multi"))
     ((eq mark mew-mark-review) (dtk-speak "review"))
     ((eq mark mew-mark-delete) (dtk-speak "delete"))
     ((eq mark mew-mark-unlink) (dtk-speak "unlink"))
     ((eq mark mew-mark-refile) (dtk-speak "refile"))
     ((eq mark mew-mark-tmp) (dtk-speak "tmp"))
     (t (dtk-speak "unmarked"))
     )
))

(defun emacspeak-mew-backward-char (&optional here)
  "Return beginning of preceding word."
  (let ((case-fold-search t)
        (start nil)
        (end (point))
        (regex (concat "[^" mew-address-separator "]")))
    (save-excursion
      (while (and (not (bobp))
                  (string-match regex (mew-buffer-substring
                                       (1- (point)) (point))))
        (forward-char -1))
      (if (and here (not (re-search-forward (regexp-quote here) end t)))
          nil ;; "here" doesn't exist.
          (setq start (point))
          start))))

(defun emacspeak-mew-get-value (&optional here)
  "Returns the beginning of previous value."
  (beginning-of-line)
  (if (not (looking-at "[^:]+:"))
      ()
    (goto-char (match-end 0))
    (if (looking-at "[ \t]")
	(forward-char 1)
      (mew-complete-insert " "))
    (if (eolp)
	nil
      (let ((start (point)))
	(end-of-line)
	(if (and here (re-search-backward (regexp-quote here) start t))
	    (progn
	      (setq start (1+ (point)))
	      (end-of-line)))
	start))))

(defun emacspeak-mew-summary-speak-line ()
  "Speaks current summary line as specified."
  (emacspeak-speak-line)
)

(defun emacspeak-mew-summary-next-line (arg)
  (interactive "p")
  (next-line arg)
  (emacspeak-mew-summary-speak-line)
)

(defun emacspeak-mew-summary-previous-line (arg)
  (interactive "p")
  (previous-line arg)
  (emacspeak-mew-summary-speak-line)
)

;;}}}
(provide 'emacspeak-mew)
