EMACS = emacs

ELFILES = \
	joxa-start.el \
	joxa.el

ELCFILES = $(ELFILES:.el=.elc)

%.elc: %.el
	$(EMACS) --batch --eval '(setq load-path (cons "." load-path))' \
		-f batch-byte-compile $<

all: $(ELCFILES)
