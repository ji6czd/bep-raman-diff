;;; bep-setup.el --- ption: Setup of the BEP for convention

;;; $Id: bep-setup.el,v 1.1 2002/02/02 16:26:46 inoue Exp $
;;; $Author: inoue $
;;; Description:  Module to set up dtk voices and personalities
;;; Keywords: Voice, Personality, BEP
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2002/02/02 16:26:46 $ |
;;;  $Revision: 1.1 $ |
;;; Location undetermined
;;;

;;}}}
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

;;; Commentary:

;;; This file is present for conventional setup for BEP users.
;;; This file call emacspeak-setup first, and setup some BEP-specific
;;; application settings.
;;; You can load BEP by simply loading this file.
;; 

;;; Code:

(defvar bep-setup-hook nil
  "hook variable called after the setup of BEP finished.")

;;; Set up Emacspeak first.
(load
 (concat (expand-file-name "emacspeak-setup"
			   (file-name-directory load-file-name))))

;;; Load Multilingual extension.
(require 'emacspeak-m17n-setup)

;;; Do additional package setup.
(emacspeak-do-package-setup "egg" 'emacspeak-egg)
(emacspeak-do-package-setup "quail" 'emacspeak-kkc)
(emacspeak-do-package-setup "liece" 'emacspeak-liece)
(emacspeak-do-package-setup "mew" 'emacspeak-mew)
(emacspeak-do-package-setup "mime-setup" 'emacspeak-semi)
(emacspeak-do-package-setup "w3m" 'emacspeak-w3m)

(run-hooks bep-setup-hook)

(provide 'bep-setup)
;;; bep-setup.el ends here
