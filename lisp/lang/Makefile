# Makefile for Emacspeak multilingual extension
#
# What emacs is called on your system?
EMACS = emacs
SRC="../.."
# How to run in batch mode
BATCH = -batch -q -no-site-file
# How to compile 
COMPILE = -eval "(setq byte-compile-dynamic t)" -f batch-byte-compile
%.elc:  %.el
	$(EMACS) $(BATCH)  -l emacspeak-m17n-load-path.el \
		$(COMPILE) $<

FRAMEWORK = emacspeak-m17n.elc emacspeak-m17n-override.elc

all: ${FRAMEWORK} modules

modules:
	(cd ja; ${MAKE} EMACS=${EMACS})

clean:
	rm -rf *.elc */*.elc
	(cd ja; ${MAKE} clean)

# File dependencies
emacspeak-m17n.elc: emacspeak-m17n.el
emacspeak-m17n-override.elc: emacspeak-m17n-override.el
