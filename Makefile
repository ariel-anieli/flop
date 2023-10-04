ERLC ?= /usr/bin/erlc
ERLR ?= /usr/bin/erl
PERL ?= /usr/bin/perl

APPLICATION ?= flop.app
DESCRIPTION ?= "Provides CLI snippets of configured links"
SRC_DIR     := src
BIN_DIR     := ebin

BIN := $(patsubst $(SRC_DIR)/%.erl,%.beam,$(wildcard $(SRC_DIR)/*.erl))
MOD := $(shell echo $(BIN) | sed -e 's/\.beam//g; s/ \(.\)/, \1/g')

vpath %.erl  $(SRC_DIR)
vpath %.beam $(BIN_DIR)
vpath %.app  $(BIN_DIR)

all: $(APPLICATION)

run: $(APPLICATION)
	$(ERLR) -pa $(BIN_DIR)/ -eval "application:start($(basename $(<F)))"

clean:
	rm -rf $(BIN_DIR)/

%.app: BIN_LIST := $(BIN:%.beam=%)
%.app: $(BIN)
	$(file >  $@, {application, $(basename $(@F)), [)
	$(file >> $@, {description, $(DESCRIPTION)},)
	$(file >> $@, {vsn, "0.1.0"},)
	$(file >> $@, {modules, [$(MOD)]},)
	$(file >> $@, {registered, [$(filter $(basename $(@F))_sup,$(BIN_LIST))]},)
	$(file >> $@, {applications, [kernel, stdlib]},)
	$(file >> $@, {mod, {$(filter $(basename $(@F))_app,$(BIN_LIST)), []}})
	$(file >> $@, ]}.)

	@mv $@ $(BIN_DIR)/

%.beam: %.erl | $(BIN_DIR)
	$(ERLC) -W0 -o $|/ $^

$(BIN_DIR):
	@mkdir $@

help:
	@echo "make all: compile all Erlang modules."
	@echo "make clean: clean up the compiled modules."
	@echo "make run: run REPL with the compiled modules."

.DEFAULT_GOAL := all
.PHONY:    clean help run
.SUFFIXES:
.SUFFIXES: .erl .app .beam
