;;; emacspeak-mew.el --- Speech enable Mew -- Fluent spoken access to internet message
;;; $Id: emacspeak-mew.el,v 1.2 2004/11/28 09:13:40 inoue Exp $
;;; $Author: inoue $ 
;;; Description:  Emacspeak extension to speech enable Mew
;;; Keywords: Emacspeak, Mew, mail, Advice, Spoken Output
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date: 2004/11/28 09:13:40 $ |
;;;  $Revision: 1.2 $ | 
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
  ;; (require 'mew)
  (require 'dtk-speak)
  (require 'voice-setup)
  ;; (require 'mew-vars2)
  ;; (require 'mew-summary)
  ;; (require 'mew-message)
  ;; (require 'mew-virtual)
  ;; (require 'mew-mark)
)
(require 'emacspeak-speak)

;(require 'emacspeak-sounds)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{ variables

(defvar emacspeak-mew-summary-field-names
  '((mark . "mark")
    (type . "type")
    (date . "date")
    (time . "time")
    (from . "from")
    (t . "thread indent")
    (subj . "subject")
    (body . "body")
    (all . "all"))
  "Map from symbols used in mew-summary-form to its corresponding name to be spoken.
'all is special and corresponds to whole summary line.")

(defvar emacspeak-mew-mark-multi
  (if (boundp 'mew-mark-multi) mew-mark-multi ?@)
  "Mew ver 3 or older has mew-mark-multi but ver 4 or later not.
This is hack to make this code compatible.")

(defvar emacspeak-mew-summary-fields-for-toggle
  '(all from subj)
"Fields toggled by emacspeak-mew-summary-speak-toggle.")

;;}}

;;{{{ voices
(defvar emacspeak-mew-mark-delete-voice 'paul-monotone)
(defvar emacspeak-mew-mark-voice 'paul-monotone)
(defvar emacspeak-mew-mark-review-voice 'paul-animated)
(defvar emacspeak-mew-mark-refile-voice 'betty)
(defvar emacspeak-mew-mark-multi-voice 'paul-animated)
(defvar emacspeak-mew-cite-voice  'paul-monotone)

(defvoice mew-voice-female-monotone
  (list 'betty nil 0 nil nil)
  "female monotone voice")

(defvoice mew-voice-low-animate
  (list nil 3 8 7)
  "Lower animated voice")

(defvoice mew-voice-higher-monotone
  (list nil 7 0 0)
  "Highermonotone voice.")

(def-voice-font  emacspeak-mew-face-body-comment-personality
  mew-voice-low-animate
  'mew-face-body-comment
  "mew-face-body-comment")

(def-voice-font  emacspeak-mew-face-body-cite1-personality
  voice-monotone
  'mew-face-body-cite1
  "mew-face-body-cite1")

(def-voice-font  emacspeak-mew-face-body-cite2-personality
  mew-voice-higher-monotone
  'mew-face-body-cite2
  "mew-face-body-cite2")

(def-voice-font  emacspeak-mew-face-body-cite3-personality
  mew-voice-higher-monotone
  'mew-face-body-cite3
  "mew-face-body-cite3")

(def-voice-font  emacspeak-mew-face-body-cite4-personality
  mew-voice-higher-monotone
  'mew-face-body-cite4
  "mew-face-body-cite4")

(def-voice-font  emacspeak-mew-face-body-cite5-personality
  mew-voice-higher-monotone
  'mew-face-body-cite5
  "mew-face-body-cite5")

(def-voice-font  emacspeak-mew-face-body-url-personality
  mew-voice-female-monotone
  'mew-face-body-url
  "mew-face-body-url")

(def-voice-font  emacspeak-mew-face-mark-delete-personality
  voice-monotone
  'mew-face-mark-delete
  "mew-face-mark-delete")

(def-voice-font  emacspeak-mew-face-mark-unlink-personality
  voice-monotone
  'mew-face-mark-unlink
  "mew-face-mark-unlink")

(def-voice-font  emacspeak-mew-face-mark-refile-personality
  'betty
  'mew-face-mark-refile
  "mew-face-mark-refile")

(def-voice-font  emacspeak-mew-face-mark-review-personality
  mew-voice-female-monotone
  'mew-face-mark-review
  "mew-face-mark-review")

(def-voice-font  emacspeak-mew-face-mark-multi-personality
  mew-voice-female-monotone
  'mew-face-mark-multi
  "mew-face-mark-multi")

(def-voice-font  emacspeak-mew-face-mark-escape-personality
  mew-voice-female-monotone
  'mew-face-mark-escape
  "mew-face-mark-escape")

;;}}}

;;{{{ hook
(add-hook 'mew-init-hook
	  (function (lambda ()
		      (define-key mew-summary-mode-map "\C-erf"
			'emacspeak-mew-speak-from)
		      (define-key mew-summary-mode-map "\C-ert"
			'emacspeak-mew-speak-to)
		      (define-key mew-summary-mode-map "\C-ers"
			'emacspeak-mew-speak-subject)
		      (define-key mew-summary-mode-map "\C-erc"
			'emacspeak-mew-speak-cc)
		      (define-key mew-summary-mode-map "\C-ern"
			'emacspeak-mew-speak-newsgroups)
		      (define-key mew-summary-mode-map "\C-erm"
			'emacspeak-mew-speak-summary-status)
		      (define-key mew-message-mode-map "\C-erf"
			'emacspeak-mew-speak-from)
		      (define-key mew-message-mode-map "\C-ert"
			'emacspeak-mew-speak-to)
		      (define-key mew-message-mode-map "\C-ers"
			'emacspeak-mew-speak-subject)
		      (define-key mew-message-mode-map "\C-erc"
			'emacspeak-mew-speak-cc)
		      (define-key mew-message-mode-map "\C-ern"
			'emacspeak-mew-speak-newsgroups)
		      (define-key mew-summary-mode-map "\C-e\C-i"
			'emacspeak-mew-summary-speak-toggle)
		      (define-key mew-summary-mode-map "\C-erk"
			'emacspeak-mew-summary-speak-mark)
		      )))

(add-hook 'mew-summary-mode-hook
	  (function (lambda ()
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
		      (voice-lock-mode 1)
		      )))

(add-hook 'mew-message-hook
	  (function (lambda ()
		    (emacspeak-auditory-icon 'on)
		    ;; (dtk-speak "Displayed message.")
		    )))
				      
(add-hook 'mew-virtual-mode-hook
	  (function (lambda ()
		      (voice-lock-mode 1)
		      )))
;;}}}

;;{{{ Advise top-level Mew command
(defadvice mew (after emacspeak pre act )
  "read the mode line after Mew starts."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-mode-line)))

(defadvice mew-summary-suspend (after emacspeak pre act )
  "announces after Mew suspends."
;  (dtk-interp-queue "Mew suspended")
  (emacspeak-auditory-icon 'close-object)
  (emacspeak-speak-mode-line))

(defadvice mew-summary-quit (after emacspeak pre act )
  "announces after Mew quitss."
;  (dtk-interp-queue "Mew quit")
  (emacspeak-auditory-icon 'close-object)
  (emacspeak-speak-mode-line))

;;; If you use Old Mew(Version 2.2)
(defadvice mew-kill-buffer (after emacspeak pre act)
  "announce kill buffer"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    ;;    (emacspeak-auditory-icon 'quit)
  (emacspeak-speak-mode-line)))

;;; If you use On Mew 3.xx later
(defadvice mew-summary-kill (after emacspeak pre act)
  "announce kill buffer"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    ;;    (emacspeak-auditory-icon 'quit)
  (emacspeak-speak-mode-line)))

(defadvice mew-draft-kill (after emacspeak pre act)
  "announce kill draft buffer "
  (emacspeak-auditory-icon 'close-object)
  (emacspeak-speak-mode-line))
(defadvice mew-draft-cite (after emacspeak pre act)
  "provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object)))

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
  (emacspeak-auditory-icon 'large-movement)
  (emacspeak-speak-line)
)

(defadvice mew-summary-jump-bottom (after emacspeak pre act )
  (emacspeak-auditory-icon 'large-movement)
  "speeks the message after movement"
  (emacspeak-speak-line)
)

(defadvice mew-summary-jump-message (after emacspeak pre act )
  "speeks the message after movement"
  (when (interactive-p)
    (emacspeak-speak-line)))

(defadvice mew-summary-display-review-up (after emacspeak pre act )
  "speeks the message after movement"
  (emacspeak-mew-summary-speak-line)
)

(defadvice mew-summary-display-review-down (after emacspeak pre act )
  "speeks the message after movement"
  (emacspeak-mew-summary-speak-line)
)

(defadvice mew-summary-goto-msg-mode (after emacspeak pre act )
  "Announce move to message mode."
  (emacspeak-auditory-icon 'on)
  (emacspeak-speak-mode-line)
)

(defadvice mew-message-goto-summary (after emacspeak pre act )
  "Announce move to summary mode."
  (emacspeak-auditory-icon 'on)
  (emacspeak-mew-summary-speak-line)
)

(defadvice mew-summary-goto-folder (after emacspeak pre act )
  "speeks the message after movement"
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-speak-line)
)

(defadvice mew-summary-delete (after emacspeak pre act )
  "tells the message is marked for delete."
  (emacspeak-auditory-icon 'delete-object)
;  (dtk-interp-queue "delete")
  (emacspeak-mew-summary-speak-line)
)

(defadvice mew-summary-refile (after emacspeak pre act )
  "tells the message is marked for refile."
  (emacspeak-mew-summary-speak-line)
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
  (emacspeak-auditory-icon 'mark-object)
  (dtk-speak "review")
)

(defadvice mew-summary-escape (after emacspeak pre act )
  "tells that escape mark is put on the message."
  (emacspeak-auditory-icon 'mark-object)
  (dtk-speak "escape")
)

(defadvice mew-summary-mark-all (after emacspeak pre act )
  "Play sound to notify that something is changed."
  (emacspeak-auditory-icon 'modified-object)
)

(defadvice mew-summary-mark-delete (after emacspeak pre act )
  "Play sound to notify that something is changed."
  (emacspeak-auditory-icon 'delete-object)
)

(defadvice mew-summary-mark-swap (after emacspeak pre act)
  "Play sound to notify that something is changed."
  (emacspeak-auditory-icon 'modified-object)
)

(defadvice mew-summary-mark-refile (after emacspeak pre act )
  "Play sound to notify that something is changed."
  (emacspeak-auditory-icon 'modified-object)
)

(defadvice mew-summary-mark-escape (after emacspeak pre act )
  "Play sound to notify that something is changed."
  (emacspeak-auditory-icon 'modified-object)
)

(defadvice mew-summary-mark-review (after emacspeak pre act )
  "Play sound to notify that something is changed."
  (emacspeak-auditory-icon 'modified-object)
)

(defadvice mew-message-next-msg (after emacspeak pre act )
  "speaks the message header infomation."
  (save-excursion
    (let ((speak-header
	   (concat "From:" (mew-header-get-value "From:") "Subject:" (mew-header-get-value "Subject:")))
	  (msg (mew-current-get-msg (mew-frame-id))))
      (dtk-speak (format "%s: %s" msg speak-header))
)))

(defadvice mew-summary-analyze-again (after emacspeak pre act)
  "Automatically read message"
  (set-buffer "*Mew message*0")
  (emacspeak-speak-rest-of-buffer))


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

(defadvice mew-attach-toggle (before emacspeak pre act )
  "Announce type is toggled."
  (dtk-speak "Toggle content type")
)

(defadvice mew-summary-make-thread (around emacspeak pre act)
  "announce when Making thread"
  (dtk-speak "making thread...")
  (let ((emacspeak-speak-messages nil))
    ad-do-it)
  (emacspeak-speak-mode-line)
  ad-return-value
)
(defadvice mew-summary-thread-up (after emacspeak pre act)
  "speak the current line"
  (emacspeak-speak-line))

(defadvice mew-summary-thread-down (after emacspeak pre act)
  "speak the current line"
  (emacspeak-speak-line))

;(defadvice mew-summary-thread-parent (after emacspeak pre act)
;  "Provide auditory feedback"
;  (when (interactive-p)
;    (emacspeak-auditory-icon 'search-hit)))


;(defadvice  mew-scan-sentinel (after emacspeak pre act )
;  "Provide auditory feedback"
;  (emacspeak-auditory-icon 'task-done)
;  (let ((dtk-stop-immediately nil))
;    (dtk-speak   emacspeak-last-message )))

(add-hook 'mew-scan-sentinel-hook
	  (function (lambda ()
		      (emacspeak-auditory-icon 'task-done)
		      (let ((dtk-stop-immediately nil))
			(dtk-speak   emacspeak-last-message )))))

(add-hook 'mew-pop-sentinel-non-biff-hook
	  (function (lambda ()
		      (emacspeak-auditory-icon 'task-done)
		      (setq biff-current-message-num 0)
		      (let ((dtk-stop-immediately nil))
			(dtk-speak   emacspeak-last-message )))))

(add-hook 'mew-smtp-sentinel-hook
	  (function (lambda ()
		      (emacspeak-auditory-icon 'task-done)
		      (let ((dtk-stop-immediately nil))
			(dtk-speak   emacspeak-last-message )))))

(add-hook 'mew-summary-exec-hook

	  (function (lambda ()
		      (emacspeak-auditory-icon 'task-done)
		      (let ((dtk-stop-immediately nil))
			(dtk-speak   emacspeak-last-message ))
		      )))

(add-hook 'mew-pack-hook

	  (function (lambda ()
		      (emacspeak-auditory-icon 'task-done)
		      ;(let ((dtk-stop-immediately nil))
			;(dtk-speak   emacspeak-last-message ))
		      )))

(add-hook 'mew-sort-hook

	  (function (lambda ()
		      (emacspeak-auditory-icon 'task-done)
		      ;(let ((dtk-stop-immediately nil))
			;(dtk-speak   emacspeak-last-message ))
		      )))

;;; helper functions
(defun emacspeak-mew-mark-to-string ()
  "Translates the mark character to the string to be spoken."
  (interactive)
  (let ((mark (mew-summary-get-mark)))
    (cond
     ((eq mark emacspeak-mew-mark-multi) "multi")
     ((eq mark mew-mark-review) "review")
     ((eq mark mew-mark-delete) "delete")
     ((eq mark mew-mark-unlink) "unlink")
     ((eq mark mew-mark-refile) "refile")
     ((eq mark mew-mark-escape) "escape")
     ((eq mark mew-mark-unread) "unread")
     (t "")
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

(defun emacspeak-mew-summary-speak-line (&optional field)
  "Speaks current summary line as specified."
  (if (and (not (eobp))
	   (mew-summary-message-number))
      (let* ((f (or field
		    (car emacspeak-mew-summary-fields-for-toggle)))
	     (start (if (eq f 'all)
			0
		      (emacspeak-mew-calculate-cols f))))
	(save-excursion
	  (move-to-column start)
;	  (dtk-interp-queue (emacspeak-mew-mark-to-string))
	  (emacspeak-speak-line 1)
	  ))
    (emacspeak-speak-line)
    ))

(defun emacspeak-mew-summary-speak-toggle ()
  "Toggle what is spoken in summary mode."
  (interactive)
  (let* ((field (car emacspeak-mew-summary-fields-for-toggle))
	 f)
    (setq emacspeak-mew-summary-fields-for-toggle
	  (append (cdr emacspeak-mew-summary-fields-for-toggle)
		  (cons field nil)))
    (setq field (car emacspeak-mew-summary-fields-for-toggle))
    (setq f (assq field emacspeak-mew-summary-field-names))
    (dtk-speak (cdr f))
    ))

(defun emacspeak-mew-summary-next-line (arg)
  (interactive "p")
  (when (and (interactive-p)
             (save-excursion
               (end-of-line)
               (eobp)))
    (emacspeak-auditory-icon 'warn-user))
  (next-line arg)
  (emacspeak-mew-summary-speak-line))

(defun emacspeak-mew-summary-previous-line (arg)
  (interactive "p")
  (when (and (interactive-p)
             (save-excursion
               (beginning-of-line)
               (bobp)))
    (emacspeak-auditory-icon 'warn-user))
  (previous-line arg)
  (emacspeak-mew-summary-speak-line)
)

(defun emacspeak-mew-speak-header (header)
  "Speak specified header"
  (save-excursion
    (set-buffer "*Mew message*0")
    (let ((header-string (mew-header-get-value header)))
      (if header-string
	  (dtk-speak (format "%s %s" header header-string))
	(dtk-speak "Not found.")
	))))

(defun emacspeak-mew-speak-from ()
  (interactive)
  (emacspeak-mew-speak-header "From:"))

(defun emacspeak-mew-speak-subject ()
  (interactive)
  (emacspeak-mew-speak-header "Subject:"))

(defun emacspeak-mew-speak-to ()
  (interactive)
  (emacspeak-mew-speak-header "To:"))

(defun emacspeak-mew-speak-cc ()
  (interactive)
  (emacspeak-mew-speak-header "Cc:"))

(defun emacspeak-mew-speak-newsgroups ()
  (interactive)
  (emacspeak-mew-speak-header "Newsgroups:"))

(defun emacspeak-mew-speak-summary-status ()
  "Speaks status string on the mode line."
  (interactive)
  (let (str)
    (setq str
	  (or mew-summary-buffer-left-msgs
	      ""))
    (if mew-summary-buffer-process
	(setq str (concat str " " mew-summary-buffer-process-status)))
    (dtk-speak str))
)

(defun emacspeak-mew-summary-speak-mark ()
  "Speaks mark of current summary line."
  (interactive)
  (dtk-speak (emacspeak-mew-mark-to-string)))

;; Sound biff
(setq biff-current-message-num 0)
(defun mew-biff-bark (n)
  (if (= n 0)
      (setq mew-biff-string nil)
    (if (and mew-use-biff-bell (> n biff-current-message-num))
	(emacspeak-auditory-icon 'new-mail))
    (setq mew-biff-string (format "Mail(%d)" n)))
  (setq biff-current-message-num n))

;; Helper for emacspeak-mew-summary-speak-line
(defun emacspeak-mew-calculate-cols (field)
  (let* ((form (if (boundp 'mew-summary-form)
		   (append mew-summary-form-header mew-summary-form)
		 (append mew-scan-form-header mew-scan-form)))
	(cols 0)
	(curr (car form))
	(f (if (consp curr)
	       (cadr curr) curr))
	ti)
    (while (not (eq f field))
      (cond
       ((not curr)
	(error (format "No field matching to %s." (symbol-name field))))
       ((consp curr)
	(setq cols (+ cols (abs (car curr)))))
       ((stringp curr)
	(setq cols (+ cols (length curr))))
       ((eq t curr) ;; thread indent
	(save-excursion
	  (move-to-column cols)
	  (setq ti (or
		    (get-text-property (point)
				       'mew-thread-indent)
		    0)))
	(if (boundp 'mew-thread-indent-strings)
	    (setq cols
		  (+ cols (* ti (length (aref mew-thread-indent-strings 0)))))
	  (setq cols
		(+ cols (* ti (length mew-thread-indent-string)))))
	)
       (t
	(setq cols (1+ cols))))
      (setq form (cdr form))
      (setq curr (car form))
      (setq f (if (consp curr)
		  (cadr curr) curr))
      )
    cols
    )
)

;;}}}
(provide 'emacspeak-mew)
