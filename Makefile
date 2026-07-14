.PHONY: all clean lint compile test

EMACS ?= emacs
BATCH = $(EMACS) -Q --batch --eval "(package-initialize)" -L .

all: compile

compile:
	$(BATCH) -f batch-byte-compile pgx-mode.el

lint:
	$(BATCH) \
		--eval "(require 'checkdoc)" \
		--eval '(checkdoc-file "pgx-mode.el")'

clean:
	rm -f *.elc

test: compile
	$(BATCH) -l test-pgx-mode.el
