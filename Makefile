ERLC ?= /usr/bin/erlc
ERLR ?= /usr/bin/erl
PERL ?= /usr/bin/perl

SRC_DIR := src
BIN_DIR := ebin

BIN := $(patsubst $(SRC_DIR)/%.erl,%.beam,$(wildcard $(SRC_DIR)/*.erl))
MOD := $(shell echo $(BIN) | sed -e 's/\.beam//g; s/ \(.\)/, \1/g')

vpath %.erl  $(SRC_DIR)
vpath %.beam $(BIN_DIR)
vpath %.app  $(BIN_DIR)

.SUFFIXES:
.SUFFIXES: .erl .app .beam

all: $(BIN) flop.app

run: $(BIN) flop.app
	$(ERLR) -pa $(BIN_DIR)/

clean:
	rm -f $(BIN_DIR)/*.beam

%.app: $(BIN)
	$(PERL) -i -pe 's/(?<=modules, \[).*(?=\])/$(MOD)/' $(BIN_DIR)/$@

%.beam: %.erl
	$(ERLC) -W0 -o $(BIN_DIR)/ $^

help:
	echo "make all: compile all Erlang modules."
	echo "make clean: clean up the compiled modules."
	echo "make run: run REPL with the compiled modules."

.SILENT: help
.PHONY: clean help run
.AUTO_GOAL: all
