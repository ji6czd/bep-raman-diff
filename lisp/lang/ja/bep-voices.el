;;; bep-voices.el --- Define various device independent voices in terms of bep-ss codes.
;;; $Id: bep-voices.el,v 1.2 2004/11/28 09:13:39 inoue Exp $
;;; $Author: inoue $
;;; Description:  Module to set up dtk voices and personalities
;;; Keywords: Voice, Personality, Dectalk
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2002, T. V. Raman 
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Introduction:

;;; Commentary:
;;; This module defines the various voices used in voice-lock mode.
;;; This module is nep-ss specific.

;;}}}
;;{{{ required modules

;;; Code:
(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'acss-structure)
;;}}}
;;{{{  voice table

(defvar tts-default-voice 'paul 
  "Default voice used. ")

(defvar bep-default-voice-string ""
  "dtk string for  default voice --set to be a no-op.")

(defvar bep-voice-table (make-hash-table)
  "Association between symbols and strings to set dtk voices.
The string can set any dtk parameter.")

(defsubst bep-define-voice (name command-string)
  "Define a dtk voice named NAME.
This voice will be set   by sending the string
COMMAND-STRING to the Dectalk."
  (declare (special bep-voice-table ))
  (puthash  name command-string  bep-voice-table))

(defsubst bep-get-voice-command (name)
  "Retrieve command string for  voice NAME."
  (declare (special bep-voice-table ))
  (cond
   ((listp name)
    (mapconcat #'bep-get-voice-command name " "))
   (t (or  (gethash name bep-voice-table)
           bep-default-voice-string))))

(defsubst bep-voice-defined-p (name)
  "Check if there is a voice named NAME defined."
  (declare (special bep-voice-table ))
  (gethash name bep-voice-table ))

;;}}}
;;{{{ voice definitions

;;; the nine predefined voices:
(bep-define-voice 'paul  "[ `v1 ]")
(bep-define-voice 'harry "[ `v1 `vh65 `vb50 ]")
(bep-define-voice 'dennis "[ `v1  `vb0 ]")
(bep-define-voice 'frank "[ `v1 `vr100 ]")
(bep-define-voice 'betty "[ `v7 ]")
(bep-define-voice 'ursula "[ `v2 ]")
(bep-define-voice 'rita "[ `v2 `vr100 ]")
(bep-define-voice 'wendy "[ `v2 `vy50 ]")
(bep-define-voice 'kit "[ `v3 ]")

;;}}}
;;{{{  the inaudible voice

(bep-define-voice 'inaudible "")

;;}}}
;;{{{  Mapping css parameters to dtk codes

;;{{{ voice family codes

(defvar bep-family-table nil
  "Association list of dtk voice names and control codes.")

(defsubst bep-set-family-code (name code)
  "Set control code for voice family NAME  to CODE."
  (declare (special bep-family-table))
  (when (stringp name)
    (setq name (intern name)))
  (setq bep-family-table
        (cons (list name code )
              bep-family-table)))

(defsubst bep-get-family-code (name)
  "Get control code for voice family NAME."
  (declare (special bep-family-table ))
  (when (stringp name)
    (setq name (intern name )))
  (or (cadr (assq  name bep-family-table))
      ""))

(bep-set-family-code 'paul " `v1 ")
(bep-set-family-code 'harry " `v1 `vh65 `vb50 ")
(bep-set-family-code 'dennis " `v1  `vb0 ")
(bep-set-family-code 'frank " `v1 `vr100 ")
(bep-set-family-code 'betty " `v7 ")
(bep-set-family-code 'ursula " `v2 ")
(bep-set-family-code 'wendy " `v2 `vy50 ")
(bep-set-family-code 'rita " `v2 `vr100 ")
(bep-set-family-code 'kid " `v3 ")

;;}}}
;;{{{  hash table for mapping families to their dimensions

(defvar bep-css-code-tables (make-hash-table)
  "Hash table holding vectors of dtk codes.
Keys are symbols of the form <FamilyName-Dimension>.
Values are vectors holding the control codes for the 10 settings.")

(defsubst bep-css-set-code-table (family dimension table)
  "Set up voice FAMILY.
Argument DIMENSION is the dimension being set,
and TABLE gives the values along that dimension."
  (declare (special bep-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (puthash  key table bep-css-code-tables )))

(defsubst bep-css-get-code-table (family dimension)
  "Retrieve table of values for specified FAMILY and DIMENSION."
  (declare (special bep-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (gethash key bep-css-code-tables)))

;;}}}
;;{{{ volume

;;; Note:volume settings not implemented for Dectalks.
(defvar bep-gain-table (make-vector  10 "")
  "Maps CSS volume settings to actual synthesizer codes.")

;;}}}
;;{{{  average pitch

;;; Average pitch for standard male voice is 122hz --this is mapped to
;;; a setting of 5.
;;; Average pitch varies inversely with speaker head size --a child
;;; has a small head and a higher pitched voice.
;;; We change parameter head-size in conjunction with average pitch to
;;; produce a more natural change on the Dectalk.

;;{{{  paul average pitch

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " `vb%s `vh%s "
                    (second setting)
                    (third setting)))))
   '(
     (0 0 90)
     (1 13 81 )
     (2 26 72)
     (3 39 63)
     (4 52 54  )
     (5 65 50)
     (6 74 40)
     (7 83 30 )
     (8 87 26)
     (9 92 21)))
  (bep-css-set-code-table 'paul 'average-pitch table ))

;;}}}
;;{{{  harry average pitch
;;; Harry  has a big head --and a lower pitch for the middle setting 

(let ((table (make-vector 10 "")))

  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " `vb%s `vh% s"
                    (second setting)
                    (third setting)))))
   '(
     (0 0 90)
     (1 10 85 )
     (2 20 80)
     (3 30 70)
     (4 40 60)
     (5 50 60)
     (6 60 50)
     (7 70 40 )
     (8 80 30)
     (9 90 20)))
  (bep-css-set-code-table 'harry 'average-pitch table ))

;;}}}
;;{{{  betty average pitch

;;;defalt baseline is average pitch of 81 

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
	    (first setting)
	    (format " `vb%s `vh% s"
		    (second setting)
		    (third setting)))))
   '(
     (0 5 70)
     (1 17 66)
     (2 33 62)
     (3 49 58)
     (4 65 54 )
     (5 81  50)
     (6 85 55)
     (7 89  60)
     (8 93 65)
     (9 97 69)))
  (bep-css-set-code-table 'betty 'average-pitch table ))

;;}}}

(defsubst bep-get-average-pitch-code (value family)
  "Get  AVERAGE-PITCH for specified VALUE and  FAMILY."
  (or family (setq family 'paul))
  (if value 
      (aref (bep-css-get-code-table family 'average-pitch)
	    value)
    ""))

;;}}}
;;{{{  pitch range

;;;  Standard pitch range is 30 and is  mapped to
;;; a setting of 5.
;;; A value of 0 produces a flat monotone voice --maximum value of 100
;;; produces a highly animated voice.

;;{{{  paul pitch range

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
	    (first setting)
	    (format " `vf%s  "
		    (second setting)))))
   '(
     (0 0 )
     (1 5 )
     (2  15)
     (3  20)
     (4  25 )
     (5  30 )
     (6  47)
     (7  64)
     (8  81)
     (9  100)))
  (bep-css-set-code-table 'paul 'pitch-range table ))

;;}}}
;;{{{  harry pitch range

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
	    (first setting)
	    (format " `vf%s  "
		    (second setting)))))
   '(
     (0 0 )
     (1 5 )
     (2  15)
     (3  20)
     (4  25 )
     (5  30 )
     (6  47)
     (7  64)
     (8  81)
     (9  100)))
  (bep-css-set-code-table 'harry 'pitch-range table ))

;;}}}
;;{{{  betty pitch range

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " `vf%s  "
                    (second setting)))))
   '(
     (0 0 )
     (1 5 )
     (2  15)
     (3  20)
     (4  25 )
     (5  30 )
     (6  47)
     (7  64)
     (8  81)
     (9  100)))
  (bep-css-set-code-table 'betty 'pitch-range table ))

;;}}}
(defsubst bep-get-pitch-range-code (value family)
  "Get pitch-range code for specified VALUE and FAMILY."
  (or family (setq family 'paul))
  (if value 
      (aref (bep-css-get-code-table family 'pitch-range)
	    value)
    ""))

;;}}}
;;{{{  stress

;;; On the outloud we map stress to roughness
;;{{{  paul stress

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table (first setting)
            (format " `vr%s " (second setting)))))
   '(
     (0 0 )
     (1 0 )
     (2  0)
     (3  0)
     (4  0 )
     (5  0 )
     (6  5)
     (7  10)
     (8  15)
     (9  20)
     ))
  (bep-css-set-code-table 'paul 'stress table)
  (bep-css-set-code-table 'harry 'stress table)
  (bep-css-set-code-table 'betty  'stress table))

;;}}}
(defsubst bep-get-stress-code (value family)
  (or family (setq family 'paul ))
  (if value 
      (aref (bep-css-get-code-table family 'stress)
	    value)
    ""))

;;}}}
;;{{{  richness

;;{{{  paul richness

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " `vv%s "
                    (second setting)))))
   '(
     (0 60)
     (1 78)
     (2 80)
     (3 84)
     (4 88)
     (5 92)
     (6 93)
     (7 95)
     (8 97 )
     (9 100)))
  (bep-css-set-code-table 'paul 'richness table)
  (bep-css-set-code-table 'harry 'richness table)
  (bep-css-set-code-table 'betty 'richness table))

;;}}}

(defsubst bep-get-richness-code (value family)
  (or family (setq family 'paul))
  (if value 
      (aref (bep-css-get-code-table family 'richness)
	    value)
    ""))

;;}}}

;;}}}
;;{{{  bep-define-voice-from-speech-style

(defun bep-define-voice-from-speech-style (name style)
  "Define NAME to be a bep-ss voice as specified by settings in STYLE."
  (let* ((family(acss-family style))
	 (command
	  (concat 
	   "["
	   (bep-get-family-code family)
	   (bep-get-average-pitch-code (acss-average-pitch style) family)
	   (bep-get-pitch-range-code (acss-pitch-range style) family)
	   (bep-get-stress-code (acss-stress style ) family)
	   (bep-get-richness-code (acss-richness style) family)
	   "]")))
    (bep-define-voice name command)))

;;}}}
;;{{{ list voices 

(defun bep-list-voices ()
  "List defined voices."
  (declare (special bep-voice-table))
  (loop for k being the hash-keys of bep-voice-table 
	collect   k))

;;}}}
;;{{{ configurater 

(defun bep-configure-tts ()
  "Configures TTS environment to use Dectalk family of synthesizers."
  (declare (special  bep-default-speech-rate
                     tts-default-speech-rate))
  (fset 'tts-list-voices 'bep-list-voices)
  (fset 'tts-voice-defined-p 'bep-voice-defined-p)
  (fset 'tts-get-voice-command 'bep-get-voice-command)
  (fset 'tts-voice-defined-p 'bep-voice-defined-p)
  (fset 'tts-define-voice-from-speech-style 'bep-define-voice-from-speech-style)
  (setq tts-default-speech-rate bep-default-speech-rate))

;;}}}
(provide 'bep-voices)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
