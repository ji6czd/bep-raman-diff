;;; emacspeak-m17n-override.el --- m17n-extended functions

;; Copyright (C) 2001 -- 2002, Bilingual Emacspeak Project

;; Author: Koichi INOUE <inoue@argv.org>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains modified version of functions in emacspeak-speak.el.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'dtk-speak)
  (require 'emacspeak-speak)
;  (require 'emacspeak-advice)
  (require 'emacspeak-redefine)
)
(declaim  (optimize  (safety 0) (speed 3)))

;;{{ From dtk-speak
(defvar dtk-program
  (or  (getenv "DTK_PROGRAM" ) "dtk-exp")
  "The program to use to talk to the speech engine.
Possible choices at present:
dtk-exp     For the Dectalk Express.
dtk-mv      for the Multivoice and older Dectalks.
outloud     For IBM ViaVoice Outloud
bep-ss      For Japanese/English multilingual engine
The default is dtk-exp.")

(defvar dtk-speaker-process-coding-system nil
"Coding system to use for process I/O with the speech server.")

(defvar dtk-default-language nil
  "Default language used when no property is set.")

(defsubst dtk-char-to-speech (char &optional lang)
  "Translate CHAR to speech string."
  (declare (special dtk-character-to-speech-table))
  (let (table table-sym)
    (when lang
	(setq table-sym
	      (intern-soft
	       (concat "dtk-character-to-speech-table-" (symbol-name lang)))))
    (if (and lang (eval table-sym))
	(setq table (eval table-sym))
      (setq table dtk-character-to-speech-table))
    (if (> char 127 ) ; may be problematic with European characters!
	(format "octal %o"  char )
      (aref table char ))))

(defun tts-configure-synthesis-setup (&optional tts-name)
  "Setup synthesis environment. "
  (declare (special dtk-program
                    tts-voice-reset-code
		    dtk-speaker-process-coding-system
		    ))
  (unless tts-name (setq tts-name dtk-program))
  (cond
   ((string-match "bep-ss" tts-name)
    (setq dtk-speaker-process-coding-system '(shift_jis-unix shift_jis-unix))
    (apply 'set-process-coding-system dtk-speaker-process dtk-speaker-process-coding-system)
    (let ((ja-dir
	   (expand-file-name
	    "lang/ja" emacspeak-lisp-directory)))
      (require 'bep-voices (concat ja-dir "/bep-voices"))
      (bep-configure-tts)
      ))
   ((string-match "outloud" tts-name)
    (outloud-configure-tts))
   (t (dectalk-configure-tts)))
  (load-library "voice-setup")
  (setq tts-voice-reset-code (tts-get-voice-command tts-default-voice)))
;;}}
;;{{ From dtk-tcl
(defsubst dtk-letter (letter &optional lang)
  "Speak a LETTER."
  (declare (special dtk-speaker-process
                    dtk-speak-server-initialized
                    dtk-quiet
		    dtk-default-language))
  (unless dtk-quiet
    (when dtk-speak-server-initialized
      (when lang
	(emacspeak-m17n-set-language lang))
      (dtk-interp-letter  letter )
      (when lang
	(emacspeak-m17n-set-language dtk-default-language))
      )))

(defsubst  dtk-quote(mode )
  (declare (special dtk-cleanup-patterns))
  (let ((lang (emacspeak-m17n-get-language-property (point-min)))
	fix-characters-func)
  (goto-char (point-min))
  (cond
   ((and (and lang (not (eq lang 'en)))
	 (setq fix-characters-func
	       (intern-soft
		(concat "dtk-fix-characters-" (symbol-name lang)))))
    (funcall fix-characters-func mode))
   (t
    ;;; dtk will think it's processing a command otherwise:
  (dtk-fix-brackets mode)
  ))
  ;;; fix control chars
  (dtk-fix-control-chars)
  ;;; Fix language property
  (when lang
       (put-text-property (point-min) (point-max) 'emacspeak-language lang))
))

(defsubst dtk-format-text-and-speak (start end )
  "Format and speak text.
Arguments START and END specify region to speak."
  (declare (special voice-lock-mode dtk-speaker-process
                    emacspeak-use-auditory-icons
		    dtk-default-language))
  (when (and emacspeak-use-auditory-icons
             (get-text-property start 'auditory-icon))
    (emacspeak-queue-auditory-icon (get-text-property start 'auditory-icon)))
  ;;; Check language property
  (let ((lang (or (get-text-property start 'emacspeak-language)
		  dtk-default-language
		  'en)))		; en is default
    (emacspeak-m17n-set-language lang))
  (dtk-interp-queue (format "%s\n" tts-voice-reset-code))
  (cond
   (voice-lock-mode
    (let ((last  nil)
          (personality (get-text-property start 'personality )))
      (while (and (< start end )
                  (setq last
                        (next-single-property-change  start 'personality
                                                      (current-buffer) end)))
        (if personality
            (dtk-speak-using-voice personality
                                   (buffer-substring start last ))
          (dtk-interp-queue (buffer-substring  start last)))
        (setq start  last
              personality
	      (get-text-property last  'personality))) ; end while
      ))					       ; end clause
   (t (dtk-interp-queue (buffer-substring start end  ))))
  (let ((lang (or dtk-default-language
		  'en)))		; en is default
    (emacspeak-m17n-set-language lang)))

(defun dtk-speak (text &optional ignore-skim force-language)
  "Speak the TEXT string on the  tts.
This is achieved by sending the text to the speech server.
No-op if variable `dtk-quiet' is set to nil.
If option `outline-minor-mode' is on and selective display is in effect,
only speak upto the first ctrl-m."
  (declare (special dtk-speaker-process dtk-stop-immediately
                    dtk-speech-rate
                    dtk-speak-nonprinting-chars
                    dtk-speak-treat-embedded-punctuations-specially
                    dtk-quiet  dtk-chunk-separator-syntax
                    voice-lock-mode   dtk-punctuation-mode
                    dtk-split-caps 
                    emacspeak-pronounce-pronunciation-table
                    selective-display ))
                                        ; ensure  the process  is live
  (unless (or (eq 'run (process-status dtk-speaker-process ))
              (eq 'open (process-status dtk-speaker-process )))
    (dtk-initialize))
                                        ; If you dont want me to talk,
                                        ;or my server is not
                                        ;running, I will remain silent.
  (unless  
      (or dtk-quiet
          (not dtk-speak-server-initialized))
                                        ; flush previous speech if asked to
    (when dtk-stop-immediately (dtk-stop ))
    (or (stringp text) (setq text (format "%s" text )))
    (when selective-display
      (let ((ctrl-m (string-match "\015" text )))
        (and ctrl-m
             (setq text (substring  text 0 ctrl-m ))
             (emacspeak-auditory-icon 'ellipses))))
    (when force-language
      (put-text-property 0 (length text)
			 'emacspeak-language force-language
			 text))
    (when (not (and (get-text-property 0 'emacspeak-language text)
		    (get-text-property (1- (length text)) 'emacspeak-language text)))
      (setq text (emacspeak-m17n-put-language-string-internal text)))
    (let ((lstart 0)
	  (lend nil)
	  (lmax (length text)))
      (while (and (< lstart lmax)
		  (setq lend (next-single-property-change lstart
 							  'emacspeak-language
							  text lmax)))
    (let ((subtext (substring text lstart lend))
	  (inhibit-point-motion-hooks t)
          (invisibility-spec buffer-invisibility-spec)
	  (syntax-table (syntax-table ))
          (inherit-speaker-process dtk-speaker-process)
          (pronunciation-table emacspeak-pronounce-pronunciation-table)
          (use-auditory-icons emacspeak-use-auditory-icons)
          (inherit-chunk-separator-syntax dtk-chunk-separator-syntax )
          (inherit-speak-nonprinting-chars dtk-speak-nonprinting-chars)
          (complement-separator(dtk-complement-chunk-separator-syntax ))
          (speech-rate dtk-speech-rate)
          (dtk-scratch-buffer (get-buffer-create " *dtk-scratch-buffer* "))
          (start 1)
          (end nil )
          (mode dtk-punctuation-mode)
          (split-caps dtk-split-caps)
          (voice-lock voice-lock-mode ))
      (save-excursion
        (set-buffer dtk-scratch-buffer )
        (let ((inhibit-read-only t))
          (erase-buffer)
                                        ; inherit environment
          (setq dtk-chunk-separator-syntax inherit-chunk-separator-syntax
                dtk-speaker-process inherit-speaker-process
                dtk-speech-rate speech-rate
                emacspeak-use-auditory-icons use-auditory-icons
                dtk-punctuation-mode mode
                dtk-split-caps split-caps
                dtk-speak-nonprinting-chars inherit-speak-nonprinting-chars
                voice-lock-mode voice-lock)
          (set-syntax-table syntax-table )
          (insert  subtext)
          (delete-invisible-text)
          (when pronunciation-table
            (emacspeak-pronounce-apply-pronunciations pronunciation-table))
          (dtk-handle-repeating-patterns mode)
          (dtk-quote mode))
        (goto-char (point-min))
        (skip-syntax-forward inherit-chunk-separator-syntax)
        (while (and (not (eobp))
                    (dtk-move-across-a-chunk
                     inherit-chunk-separator-syntax
                     complement-separator))
                                        ;if we matched a punctuation,
                                        ;treat this as a chunk only if the punctuation is followed
                                        ;by white space
          ;dtk-speak-treat-embedded-punctuations-specially
          ;has been T for a long time
          (unless
              (and (char-after  (point))
                   (= (char-syntax (preceding-char )) ?.)
                   (not (= 32 (char-syntax (following-char )))))
            (setq end (point ))
            (dtk-format-text-and-speak  start end )
            (setq start  end)))         ; end while
                                        ; process trailing text
        (or  (= start (point-max))
             (dtk-format-text-and-speak start (point-max)))))
    (setq lstart lend)))
    (dtk-force)))

;;}}
;;{{ From emacspeak-speak
(defun emacspeak-speak-this-char (char &optional lang)
  "Speak this CHAR."
  (let ((dtk-stop-immediately t )
	(lang
	 (or lang 'en)))
    (when char
      (emacspeak-handle-action-at-point)
      (cond
       ((and (not (eq lang 'en)) 	; chara with language property
	     (< char 256))		; and less than 256
	;; treat char as original emacspeak does,
	;; but speak with specified lang.
	(cond
	 ((emacspeak-is-alpha-p char)
	  (dtk-letter (char-to-string char ) lang))
	 (t
	  (dtk-speak (dtk-char-to-speech char lang) nil lang))))
       ((not (eq lang 'en))		; chara with language property
	(dtk-speak (emacspeak-m17n-get-cursor-string char lang) nil lang))
       ((emacspeak-is-alpha-p char) (dtk-letter (char-to-string char )))
       (t (dtk-dispatch
           (dtk-char-to-speech char )))))))

(defun emacspeak-speak-char (&optional prefix)
  "Speak character under point.
Pronounces character phonetically unless  called with a PREFIX arg."
  (interactive "P")
  (declare (special dtk-default-language))
  (let ((dtk-stop-immediately t )
        (char  (following-char ))
	(lang (or (get-text-property (point) 'emacspeak-language)
		  dtk-default-language
		  'en)))
    (when char
      (emacspeak-handle-action-at-point)
      (cond
       ((and (not (eq lang 'en)) 	; chara with language property
	     (< char 256))		; and less than 256
	;; treat char as original emacspeak does,
	;; but speak with specified lang.
	(cond
	 ((and (not prefix)
	       (emacspeak-is-alpha-p char))
	  (dtk-speak (emacspeak-get-phonetic-string char ) nil lang))
	 ((emacspeak-is-alpha-p char)
	  (dtk-letter (char-to-string char ) lang))
	 (t
	  (dtk-speak (dtk-char-to-speech char lang) nil lang))))
       ((not (eq lang 'en))		; general non-en chars
	(if (not prefix)
	    (dtk-speak (emacspeak-m17n-get-phonetic-string char lang) nil lang)
	  (dtk-speak (emacspeak-m17n-get-cursor-string char lang) nil lang)))
       ;; for 'en properties
       ((and (not prefix)
             (emacspeak-is-alpha-p char))
        (dtk-speak (emacspeak-get-phonetic-string char )))
       ((emacspeak-is-alpha-p char) (dtk-letter (char-to-string char )))
       (t (dtk-dispatch
           (dtk-char-to-speech char )))))))

(defun emacspeak-speak-buffer (&optional arg)
  "Speak current buffer  contents.
With prefix ARG, speaks the rest of the buffer from point.
Negative prefix arg speaks from start of buffer to point.
 If voice lock mode is on, the paragraphs in the buffer are
voice annotated first,  see command `emacspeak-speak-voice-annotate-paragraphs'."
  (interactive "P" )
  (declare (special emacspeak-speak-voice-annotated-paragraphs))
  (when (and voice-lock-mode
             (not emacspeak-speak-voice-annotated-paragraphs))
    (emacspeak-speak-voice-annotate-paragraphs))
  (when (listp arg) (setq arg (car arg )))
  (let ((start nil )
        (end nil))
    (cond
     ((null arg)
      (setq start (point-min)
            end (point-max)))
     ((> arg 0)
      (setq start (point)
            end (point-max)))
     (t (setq start (point-min)
              end (point))))
    (emacspeak-m17n-put-language-region start end)
    (dtk-speak (buffer-substring start end ))))
;;}}

;;{{ From emacspeak-redefine
(defun emacspeak-self-insert-command (arg)
  "Insert a character.
Speaks the character if emacspeak-character-echo is true.
See  command emacspeak-toggle-word-echo bound to
\\[emacspeak-toggle-word-echo].
Toggle variable dtk-stop-immediately-while-typing if you want to have
speech flush as you type."
  (interactive "p")
  (declare (special last-input-char
                    dtk-stop-immediately-while-typing dtk-program 
                    buffer-undo-list  buffer-read-only
                    emacspeak-character-echo
                    emacspeak-word-echo
		    emacspeak-character-echo-non-ascii))
  (when buffer-read-only
    (signal 'buffer-read-only
            (list (current-buffer))))
  (unless (car buffer-undo-list)
    (pop buffer-undo-list ))
  (self-insert-command  arg )
  (cond
   ((and emacspeak-word-echo
         (interactive-p)
	 (= last-input-char 32 ))
    (save-excursion
      (condition-case nil
          (forward-word -1)
        (error nil))
      (emacspeak-speak-word)))
   ((and emacspeak-character-echo
         (interactive-p )
	 (or emacspeak-character-echo-non-ascii
	     (< last-input-char 255)))
    (when dtk-stop-immediately-while-typing (dtk-stop))
    (emacspeak-speak-this-char last-input-char
			       (emacspeak-m17n-maybe-last-input-language))))
  (and
   (= (char-syntax  last-input-char) 32)
   (>= (current-column) fill-column)
   auto-fill-function
   (funcall auto-fill-function)))
;;}}

;;{{ From emacspeak-advice
(defadvice delete-backward-char (around emacspeak pre act)
  "Speak character you're deleting."
  (let ((lang
	 (emacspeak-m17n-get-language-property (1- (point)))))
    (cond
     ((interactive-p )
      (dtk-tone 500 30 'force)
      (emacspeak-speak-this-char (preceding-char) lang)
     ad-do-it)
     (t ad-do-it))
    ) ; end of let
  ad-return-value)

(defadvice backward-delete-char-untabify (around emacspeak pre act)
  "Speak character you're deleting."
  (let ((lang
	 (emacspeak-m17n-get-language-property (1- (point)))))
    (cond
     ((interactive-p )
      (dtk-tone 500 30 'force)
      (emacspeak-speak-this-char (preceding-char) lang)
      ad-do-it)
     (t ad-do-it))
    ) ;; end of let
  ad-return-value)
;;}}

(provide 'emacspeak-m17n-override)
;;; emacspeak-m17n-overrice.el ends here
