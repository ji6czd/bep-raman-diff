# Makefile for Emacspeak multilingual extension
#
# what emacs is called on your system
EMACS = emacs
SRC="../../.."
# How to run in batch mode
BATCH = -batch -q -no-site-file
# How to compile 
COMPILE = -eval "(setq byte-compile-dynamic t)" -f batch-byte-compile
%.elc:  %.el
	$(EMACS) $(BATCH)  -l ../emacspeak-m17n-load-path.el \
		$(COMPILE) $<

EMACSPEAK_JA = \
	emacspeak-ja-setup.elc \
	bep-css-speech.elc \
	bep-voices.elc \
	emacspeak-egg.elc \
	emacspeak-ja-tbl.elc \
	emacspeak-kkc.elc \
	emacspeak-liece.elc \
	emacspeak-m17n-ja.elc \
	emacspeak-mew.elc \
	emacspeak-navi2ch.elc \
	emacspeak-semi.elc \
	emacspeak-w3m.elc \
	emacspeak-wl.elc \
	yahtml-voice.elc

all: ${EMACSPEAK_JA}

clean:
	rm -rf *.elc */*.elc


# File dependencies
