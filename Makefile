ERLC ?= $(shell whereis erlc | cut -d' ' -f2)
ERLR ?= $(shell whereis erl  | cut -d' ' -f2)

APPLICATION ?= flop.app
DESCRIPTION ?= "Provides CLI snippets of configured links"
SRC_DIR     := src
BIN_DIR     := ebin

BIN := $(patsubst $(SRC_DIR)/%.erl,%.beam,$(wildcard $(SRC_DIR)/*.erl))
MOD := $(shell echo $(BIN) | sed -e 's/\.beam//g; s/ \(.\)/, \1/g')

vpath %.erl  $(SRC_DIR)
vpath %.beam $(BIN_DIR)
vpath %.app  $(BIN_DIR)

define describe-application
{application, $(basename $(@F)), [
{description, $(DESCRIPTION)},
{vsn, "0.1.0"},
{modules, [$(MOD)]},
{registered, [$(filter $(basename $(@F))_sup,$(BIN_LIST))]},
{applications, [kernel, stdlib]},
{mod, {$(filter $(basename $(@F))_app,$(BIN_LIST)), []}}
]}.
endef

all: $(APPLICATION)

run: $(APPLICATION)
	$(ERLR) -pa $(BIN_DIR) -eval "application:start($(basename $(<F)))"

clean:
	rm -rf $(BIN_DIR)

%.app: BIN_LIST := $(patsubst %.beam,%,$(BIN))
%.app: $(BIN)
	$(file >  $@, $(describe-application))

	@mv $@ $(BIN_DIR)

%.beam: %.erl | $(BIN_DIR)
	$(ERLC) -W0 -o $| $^

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
