ERLC ?= /usr/bin/erlc
ERLR ?= /usr/bin/erl

SRC_DIR := src
BIN_DIR := ebin

vpath %.erl  $(SRC_DIR)
vpath %.beam $(BIN_DIR)

SRC := $(wildcard $(SRC_DIR)/*.erl)
BIN := $(SRC:$(SRC_DIR)/%.erl=%.beam)

all: $(BIN)

run: $(BIN)
	$(ERLR) -pa $(BIN_DIR)/

clean:
	rm -f $(BIN_DIR)/*

%.beam: %.erl
	$(ERLC) -W0 -o $(BIN_DIR)/ $^

.SILENT: 
help:
	echo "make all: compile all Erlang modules."
	echo "make clean: clean up the compiled modules."
	echo "make run: run REPL with the compiled modules."
