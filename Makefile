ERLC ?= /usr/bin/erlc
ERLR ?= /usr/bin/erl
PERL ?= /usr/bin/perl

SRC_DIR := src
BIN_DIR := ebin

vpath %.erl  $(SRC_DIR)
vpath %.beam $(BIN_DIR)
vpath %.app  $(BIN_DIR)

SRC := $(wildcard $(SRC_DIR)/*.erl)
BIN := $(SRC:$(SRC_DIR)/%.erl=%.beam)
MOD := $(shell echo $(BIN:%.beam=%) | sed -e 's/ \(.\)/, \1/g')

all: $(BIN) flop.app

run: $(BIN) flop.app
	$(ERLR) -pa $(BIN_DIR)/

clean:
	rm -f $(BIN_DIR)/*.beam

flop.app: $(BIN)
	$(PERL) -i -pe 's/(?<=modules, \[).*(?=\])/$(MOD)/' $(BIN_DIR)/$@

%.beam: %.erl
	$(ERLC) -W0 -o $(BIN_DIR)/ $^

.SILENT: help
help:
	echo "make all: compile all Erlang modules."
	echo "make clean: clean up the compiled modules."
	echo "make run: run REPL with the compiled modules."
