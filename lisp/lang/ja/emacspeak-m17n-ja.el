;;; -*- coding: iso-2022-7bit -*-
;;; emacspeak-m17n-ja.el --- Bilingual extension to emacspeak
;;; $Id: emacspeak-m17n-ja.el,v 1.2 2004/11/28 09:13:40 inoue Exp $
;;; $Author: inoue $
;;; Description: Contains functions that handle Japanese characters
;;; Keywords: Emacspeak, Japanese, multilingualization
;;;
;;; This file is not part of Emacspeak

;;;{{{  Copyright:
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

;;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Introduction:

;;; Commentary:

;;; BEP, Bilingual Emacspeak Project, extends Emacspeak to bilingual, 
;;; Japanese and English.
;;; Japanese characters shoud be encoded with Shift JIS.
;;; ShiftJIS is double-byte character.

;;}}}
;;{{
(eval-when-compile
  (require 'cl)
  (require 'dtk-speak)
  (require 'emacspeak-m17n "lang/emacspeak-m17n")
  (require 'emacspeak-ja-tbl))
;;}}
;;{{{ Variables

(defvar emacspeak-m17n-ja-strategy-sets nil
"List of available strategy set. A list of 4 element lists:
character to select this strategy,
name of the set, used for feedback messages,
strategy1 for buffer contents,
strategy2 for other things like messages.")

(setq emacspeak-m17n-ja-strategy-sets
  '((?n "Native English mode"
     emacspeak-m17n-put-language-ja-ne emacspeak-m17n-put-language-ja-ne)
    (?a "Adaptive English mode"
     emacspeak-m17n-put-language-ja-ke-1 emacspeak-m17n-put-language-ja-ne)
    (?k "Katakana English mode"
     emacspeak-m17n-put-language-ja-ke-all emacspeak-m17n-put-language-ja-ke-all)))
;;}}}
;;{{{ Access functions for auditory-display-table-ja

(defun bep-get-phonetic-string (char &optional field)
  "Return the phonetic string, the 2nd field of dic, for this CHAR."
  (interactive "sChar:\nnField:")
  (declare (special auditory-display-table-ja))
  (let ((char (or (and (integerp char) char)
		  (string-to-char char)))
	(dummy-array ["dummy" "dummy" "dummy"]))
    (or (aref
	 (or
	  (aref auditory-display-table-ja char)
	  (fillarray dummy-array (char-to-string char))) ; avoid error and return char itself
	 (if field field 0))
	" ")))

(defun bep-get-char-type (char)
  "Return the symbol of char-type for this CHAR."
  (declare (special auditory-display-table-ja))
  (let ((char (or (and (integerp char) char)
		  (string-to-char char))))
  (aref
   (or
    (aref auditory-display-table-ja char)
    [nil nil nil nil])
   3)))

(defun bep-get-cursor-string (char)
  "Return the short phonetic string (2nd field) of CHAR."
  (interactive "sChar:\n")
  (bep-get-phonetic-string char 0))

(defun bep-get-explanatory-string (char)
  "Return the explanatory string (3rd field) of CHAR."
  (interactive "sChar:\n")
  (bep-get-phonetic-string char 1))

(defun bep-get-long-explanatory-string (char)
  "Return the detail explanatory string (4th field) of CHAR."
  (interactive "sChar:\n")
  (bep-get-phonetic-string char 2))
;;}}}
;;{{
(defvar emacspeak-ja-type-prefix-table
  '(
    (ja-hiragana . "ヒラガナ")
    (ja-katakana . "カタカナ")
    (ja-alpha . "ゼンカクローマジ")
    (katakana . "ハンカク")
    (ja-other . "ゼンカク"))
  "Prefix to tell categories of Japanese characters.")

(defvar emacspeak-ja-subtype-prefix-table
  '(
    (upcase . "オオモジ")
    (small . "チイサイ"))
  "Prefix to tell attributes of Japanese characters.")

(defvar emacspeak-ja-small-char-list
  '(?ぁ ?ぃ ?ぅ ?ぇ ?ぉ ?っ ?ゃ ?ゅ ?ょ ?ァ ?ィ ?ゥ ?ェ ?ォ ?ッ ?ャ ?ュ ?ョ
	?ｱ ?ｲ ?ｳ ?ｴ ?ｵ ?ﾔ ?ﾕ ?ﾖ)
  "List of Japanese characters which has attribute `small'.")
;;}}
;;{{{ Other Japanese specific functions

(defun emacspeak-ja-convert-string-to-phonetic (str)
  "Convert string with Japanese characters to its explanatory readings."
  (declare (special emacspeak-ja-type-prefix-table
		    emacspeak-ja-subtype-prefix-table
		    emacspeak-ja-small-char-list))
  (let ((cur 0)
	(cidx 0)
	(siz (length str))
	(ret ""))
    (while (< cur siz)
      (let* ((c (aref str cur))
	     (ct (emacspeak-ja-get-char-type c))
	ptr)
	;;; get the end of region with current char type
	(setq cidx (1+ cur))
	(while (and (< cidx siz)
		    (eq ct (emacspeak-ja-get-char-type
			    (aref str cidx))))
	  (setq cidx (1+ cidx)))
	;;; make phonetic string
	(setq ret (concat ret
			  (or (cdr
			       (assoc ct emacspeak-ja-type-prefix-table))
			       "")
			   ))
	(setq ptr cur)
	(while (< ptr cidx)
	  (let* ((cptr (aref str ptr))
		 (cstr (char-to-string cptr))
		 (phon (if (memq ct '(ja-hiragana ja-katakana katakana ja-alpha alpha))
			   (bep-get-cursor-string cstr)
			   (bep-get-explanatory-string cstr)))
		 (subtype (emacspeak-ja-get-char-subtype cptr))
		 (pref (or (cdr (assoc subtype emacspeak-ja-subtype-prefix-table)) ""))
		 )
	    (setq ret (concat ret pref phon)))
	  (if (and (< ptr (1- siz)) (eq ct 'ja-kanji))
	      (setq ret (concat ret " ")))
	  (setq ptr (1+ ptr))
	  )
	(setq cur cidx)
	)
      )
    ret
    )
)

(defun emacspeak-ja-convert-char-to-phonetic (cstr)
  "convert first character of string to explanatory string with prefix indicating character subtype."
  (declare (special emacspeak-ja-type-prefix-table
		    emacspeak-ja-subtype-prefix-table
		    emacspeak-ja-small-char-list))
  (let* ((cptr (or (and (integerp cstr) cstr)
		   (string-to-char cstr)))
	 (phon (bep-get-explanatory-string cptr))
	 (ct (emacspeak-ja-get-char-type cptr))
	 (subtype (emacspeak-ja-get-char-subtype cptr))
	 (pref 
	  (or (cdr
	       (assoc ct emacspeak-ja-type-prefix-table))
	      "")
	  )
	 (spref (or (cdr (assoc subtype emacspeak-ja-subtype-prefix-table)) ""))
	 ret)
    (setq ret (concat ret pref spref phon))
    ret)
)

(defun emacspeak-ja-convert-string-to-cursor (str)
  "convert string to concatenated string of cursor-string."
  (let ((siz (length str))
	     (output "")
	     (ptr 0)
	     cstr)
    (while (< ptr siz)
      (setq cstr (substring str ptr (1+ ptr)))
      (setq output (concat output (bep-get-cursor-string cstr)))
      (setq ptr (1+ ptr))
      )
  output))

(defun emacspeak-ja-get-char-type (c)
  "returns Japanese character type"
  (declare (special auditory-disp-table-ja))
  (let ((ct (bep-get-char-type c))
	(cstr (char-to-string c)))
  (cond
   (ct ct) ; if char-type is defined in auditory-disp-table-ja
   ((string-match "[0-9]" cstr) 'number)
   ((string-match "[A-Za-z]" cstr) 'alpha)
   ((string-match "[ｦ-ﾟ]" cstr) 'katakana)
   (t nil))
))

(defun emacspeak-ja-get-char-subtype (c)
  "returns subtype of c"
  (declare (special emacspeak-ja-small-char-list))
  (cond
   ((or
     (and (>= c ?A) (<= c ?Z))
     (and (>= c ?Ａ) (<= c ?Ｚ))
     (and (>= c ?Α) (<= c ?Ω)))
    'upcase)
   ((memq c emacspeak-ja-small-char-list)
    'small)
   (t nil)
   )
)

(defun bep-is-Japanese-p (char-string)
  "return True if Japanese, i.e. double-byte character"
  (interactive)
   (equal 0 (string-match "\\cj" char-string)) ; if Japanese characters
)
;;}}}
;;{{{ Fix brackets some characters for Japanese output.
(defsubst dtk-fix-characters-ja (mode)
  "Quote any delimiters that need special treatment.
Argument MODE  specifies the current pronunciation mode.
Japanese specific."
  (declare  (special dtk-fix-characters-regexp-ja
		     dtk-replace-characters-table-ja))
  (goto-char (point-min))
  (cond
   ((string=  "all"  mode )
    (let ((start nil)
          (personality nil)
	  (language nil)
	  (c nil))
      (while (re-search-forward dtk-fix-characters-regexp-ja nil t )
        (setq start (1- (point)))
        (setq personality
              (get-text-property
               start 'personality))
	(setq language
	      (get-text-property
	       start 'emacspeak-language))
        (cond
         ((setq c (assoc (char-to-string (char-after (match-beginning 0 ))) dtk-replace-characters-table-ja))
          (replace-match (cdr c))
	  (when personality
	    (put-text-property start (point)
			       'personality personality))
	  (when language
	    (put-text-property start (point)
			       'emacspeak-language language)))
	 (t
	  (replace-match " "))))))
   (t
    (while (re-search-forward dtk-fix-characters-regexp-ja  nil t )
      (let ((language (get-text-property (match-beginning 0)
					 'emacspeak-language)))
      (replace-match " " nil t )
      (put-text-property (1- (point)) (point)
			     'emacspeak-language language))
      ))))
;;}}}
;;{{ Japanese specific put-language functions
(defun emacspeak-m17n-put-language-ja-ne (beg end &optional len)
  "Put `ja' Property to Japanese characters,
and `en' property to ASCII characters."
  (goto-char beg)
  (while (and (< (point) end)
	      (re-search-forward "\\([\t -~]+\\|\\cj+\\)" end t))
    (let* ((mbeg (match-beginning 0))
	   (mend (match-end 0))
	   (beginning (max mbeg (point-min)))
	   (ending (min mend (point-max)))
	   (lang
	    (if (string-match "^\\cj" (buffer-substring mbeg (1+ mbeg)))
		'ja
	      'en)))
      (put-text-property beginning ending 'emacspeak-language lang)
      (if (looking-at "[\011\012\014\015]+")
	  (put-text-property
	   (match-beginning 0) (match-end 0) 'emacspeak-language lang))
      ))
  (goto-char end))

(defvar emacspeak-m17n-ja-ke-limit 40
  "Non-Japanese string shorter than this value is spoken as Katakana English.")

(defvar emacspeak-m17n-ja-ke-view 3
  "The line numbers the emacspeak-m17n-put-language-ja-ke-1 looks
before and after the point, if the changed portion is shorter than
emacspeak-m17n-ja-ke-limit")

(defun emacspeak-m17n-put-language-ja-ke-1 (beg end &optional len)
  "Put `ja' Property to Japanese characters
and ascii string shorter than `emacspeak-m17n-ja-ke-limit',
and `en' property to others."
  (when (and len
	     (< len emacspeak-m17n-ja-ke-limit))
    (setq beg (line-beginning-position (- emacspeak-m17n-ja-ke-view)))
    (setq end (line-end-position emacspeak-m17n-ja-ke-view)))
  (goto-char beg)
  (while (and (< (point) end)
	      (re-search-forward "\\([\011\012\014 -~]+\\|\\(\\cj\\|\n\\)+\\)" end t))
    (let* ((mbeg (match-beginning 0))
	   (mend (match-end 0))
	   (beginning (max mbeg (point-min)))
	   (ending (min mend (point-max)))
	   (matched 
	    (buffer-substring mbeg mend)))
      (cond
       ((or (string-match "^\\cj" matched)
	    (< (length matched) emacspeak-m17n-ja-ke-limit))
	(put-text-property beginning ending 'emacspeak-language 'ja))
       (t
	(put-text-property mbeg mend 'emacspeak-language 'en))
       ) ; end of cond
      )))

(defun emacspeak-m17n-put-language-ja-ke-all (beg end &optional len)
  "Speek all characters as Japanese."
  (put-text-property beg end 'emacspeak-language 'ja))

;;}}

;;{{{

(defun emacspeak-m17n-ja-change-strategy (&optional key-char global)
  "Change put-language-strategy.
If prefix argument (GLOBAL) is provided, default value of
emacspeak-m17n-put-language-strategy for buffers and internal use are changed.
If KEY-CHAR is set to character for existing strategy-set, change values
noninteractively.
See variable emacspeak-m17n-ja-strategy-sets."
  (interactive "i\nP")
  (declare (special emacspeak-m17n-put-language-strategy
		    emacspeak-m17n-put-language-internal-strategy
		    emacspeak-m17n-ja-strategy-sets))
  (let (key strategy)
    (setq key
	  ;; read key from keyboard no key-char is provided.
	  (or key-char
	      (read-char-exclusive
	      (concat
	       (apply 'concat "Select "
		      (mapcar (function (lambda(x)
					  (format "%s(%s) "
						  (nth 1 x) ; strategy-set
						  (char-to-string (upcase (nth 0 x))) ; key
						  )))
			      emacspeak-m17n-ja-strategy-sets)) ":"))))
    ;; Check key
    (setq strategy (assoc key emacspeak-m17n-ja-strategy-sets))
    (if (not strategy)
	(message "No such strategy.")
      (emacspeak-m17n-set-put-language-strategy (nth 2 strategy))
      (if (not global)
	  (message (format "Use %s locally" (nth 1 strategy)))
	;; Set global value
	(emacspeak-m17n-set-put-language-strategy (nth 2 strategy) t)
	(emacspeak-m17n-set-put-language-internal-strategy (nth 3 strategy))
	(message (format "Use %s" (nth 1 strategy))))
      )
    ) ;; let
  (recenter))
;;}}}

(provide 'emacspeak-m17n-ja)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
