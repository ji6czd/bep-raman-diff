;;; yahtml-voice.el --- Voice locking for yahtml-mode
;;; $Id: yahtml-voice.el,v 1.2 2004/11/28 09:13:40 inoue Exp $
;;; $Author: inoue $ 
;;; Description:  Voice locking for html helper  mode. Cloned from html-font.el
;;; Keywords: voice lock, html-helper-mode, emacspeak, speech 
;;{{{  LCD Archive entry: 
;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interpersonality to Emacs |
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

;;; Code:
(require 'cl)
(eval-when (compile)
  (condition-case nil 
      (require 'yahtml)  ; When we like to modify the syntax table
    (error (message "Looks like you dont have yahtml-mode installed."))))
(require 'voice-setup)
(declaim (special yahtml-syntax-table))
;; Code for greater flexibility, especially for XEmacs compatibility.
;; Note that Emacs evaluates the personality entries in `voice-lock-keywords',
;; while XEmacs doesn't.

;;; the top level advice command
(defadvice yahtml-goto-corresponding-* (after emacspeak pre act)
  "speak html tag after movement"
  (emacspeak-speak-word))

(defadvice YaTeX-kill-buffer (around emacspeak pre act)
  (cond
   ((interactive-p)
    (dtk-speak
     (format "Kill Buffer: %s"
             (buffer-name)))
    ad-do-it
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line))
   (t ad-do-it))
  ad-return-value)

(defadvice yahtml-insert-single (after emacspeak pre act)
  "Speak inserted HTML tag."
  (dtk-speak (format "<%s>" cmd)))

(defadvice yahtml-insert-begend (after emacspeak pre act)
  "Speak inserted HTML tag."
  (dtk-speak (format "<%s>" env)))

(defadvice yahtml-insert-tag (around emacspeak pre act)
  (let ((cur (point)))
    ad-do-it
    (dtk-speak (buffer-substring cur (point)))))

(defadvice yahtml-insert-amps (around emacspeak pre act)
  (let ((cur (point)))
    ad-do-it
    (dtk-speak (buffer-substring cur (point)))))

(defadvice yahtml-insert-form (around emacspeak pre act)
  (let ((cur (point)))
    ad-do-it
    (dtk-speak (buffer-substring cur (point)))))

;;(declaim (special voice-lock-variable-name-personality))

;; Let's reset syntax to make it possible for voice-lock to colour strings.
;; They usually appear inside anchors; thus these may become two-coloured.
(modify-syntax-entry ?\" "\"   " yahtml-syntax-table)

(defvar yahtml-voice-lock-keywords
  (let ((tword "\\(h1\\|title\\)")          ; Titles, like function defs
	(bword "\\(b\\|h[2-4]\\|strong\\)") ; Names of tags to boldify
	(iword "\\(address\\|cite\\|em\\|i\\|var\\)") ; ... to italify
	;; Regexp to match shortest sequence that surely isn't a bold end.
	;; We simplify a bit by extending "</strong>" to "</str.*".
	;; Do similarly for non-italic and non-title ends.
	(not-bend (concat "\\([^<]\\|<\\([^/]\\|/\\([^bhs]\\|"
			  "b[^>]\\|"
			  "h\\([^2-4]\\|[2-4][^>]\\)\\|"
			  "s\\([^t]\\|t[^r]\\)\\)\\)\\)"))
	(not-iend (concat "\\([^<]\\|<\\([^/]\\|/\\([^aceiv]\\|"
			  "a\\([^d]\\|d[^d]\\)\\|"
			  "c\\([^i]\\|i[^t]\\)\\|"
			  "e\\([^m]\\|m[^>]\\)\\|"
			  "i[^>]\\|"
			  "v\\([^a]\\|a[^r]\\)\\)\\)\\)"))
	(not-tend (concat "\\([^<]\\|<\\([^/]\\|/\\([^ht]\\|"
			  "h[^1]\\|t\\([^i]\\|i[^t]\\)\\)\\)\\)")))
    (list
     ;; Comments: <!-- ... -->. They traditionally override string colouring.
     ;; It's complicated 'cause we won't allow "-->" inside a comment, and
     ;; voice-lock colours the *longest* possible match of the regexp.
     '("\\(<!--\\([^-]\\|-[^-]\\|--[^>]\\)*-->\\)" 1 voice-lock-comment-personality t)
     ;; SGML things like <!DOCTYPE ...> with possible <!ENTITY...> inside.
     '("\\(<![a-z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>\\)"
       1 voice-lock-comment-personality keep)
     ;; Anchors, images & forms (again keep possible string colouring inside).
     '("\\(<a\\b[^>]*>\\)" 1 voice-lock-keyword-personality keep)
     "</a>"
     '("\\(<\\(form\\|i\\(mg\\|nput\\)\\)\\>[^>]*>\\)"
       1 voice-lock-variable-name-personality keep)
     ;; HTML special characters
     '("&[^;\n]*;" . voice-lock-string-personality)
     ;; Underline is rarely used. Only handle it when no tags inside.
     '("<u>\\([^<]*\\)</u>" 1 voice-lock-underline-personality keep)
     ;; '("<u>\\([^<]*\\)<" 1 ..underl..) '("\\(>[^<]*\\)</u>" 1 ..underl..)
     ;; Titles and level 1 headings (anchors do sometimes appear in h1's)
     (list (concat "<" tword ">\\(" not-tend "*\\)</\\1>")
	   2 'voice-lock-function-name-personality 'keep)
     ;; Large-scale structure keywords (like "program" in Fortran).
     ;;   "<html>" "</html>" "<body>" "</body>" "<head>" "</head>" "</form>"
     '("</?\\(body\\|form\\|h\\(ead\\|tml\\)\\)>"
       . voice-lock-variable-name-personality)
     ;; Any tag, general rule, just before bold/italic stuff.
     '("\\(<[^>]*>\\)" 1 voice-lock-type-personality keep)
     ;; More tag pairs (<b>...</b> etc.).
     ;; Cunning repeated voiceification to handle common cases of overlap.
     ;; Bold simple --- first voiceify bold regions with no tags inside.
     (list (concat "<" bword ">\\("  "[^<]"  "*\\)</\\1>")
	   2 'voice-lock-bold-personality 'keep)
     ;; Italic complex --- possibly with arbitrary non-italic kept inside.
     (list (concat "<" iword ">\\(" not-iend "*\\)</\\1>")
	   2 'voice-lock-italic-personality 'keep)
     ;; Bold complex --- possibly with arbitrary other non-bold stuff inside.
     (list (concat "<" bword ">\\(" not-bend "*\\)</\\1>")
	   2 'voice-lock-bold-personality 'keep)))
  "Additional expressions to highlight in HTML helper mode.")

;; It shouldn't matter whether this hook is executed before or after voice-lock.
;; Fortunately, voice-lock.el is very friendly in this respect.
;; We must be equally friendly and make sure we don't make global defaults.
;; It can be done much more elegantly in Emacs 19.29+, but we'll keep it this
;; way to retain compatibility with older Emacsen.
(add-hook 'yahtml-mode-hook
 '(lambda ()
    (make-local-variable 'voice-lock-keywords-case-fold-search)
    (make-local-variable 'voice-lock-keywords)
    ;;  (make-local-variable 'voice-lock-no-comments)
    ;; Regard the patterns in yahtml-voice-lock-keywords as case-insensitive
    (setq voice-lock-keywords-case-fold-search t)
    (setq voice-lock-keywords yahtml-voice-lock-keywords)))
;;  (setq voice-lock-no-comments t)))

(provide 'yahtml-voice)

;;; html-voice.el ends here
