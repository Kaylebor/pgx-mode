.PHONY: all clean lint compile test

EMACS ?= emacs
BATCH = $(EMACS) -Q --batch

all: compile

compile:
	$(BATCH) -L . -L ~/projects/pg-el -f batch-byte-compile pgx-mode.el

lint:
	$(BATCH) -L . -L ~/projects/pg-el \
		--eval "(require 'checkdoc)" \
		-f checkdoc-file pgx-mode.el

clean:
	rm -f *.elc

test: compile
	@echo "Tests not yet implemented"