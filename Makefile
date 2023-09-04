# disable default rules and default variables
MAKEFLAGS += --no-builtin-rules --no-builtin-variables

.PHONY: all run runtests info clean

SRC := src
OBJ := obj
BIN := bin
FC := gfortran
FFLAGS := -J $(OBJ)
SOURCES := $(wildcard $(SRC)/*.f90)
OBJECTS := $(SOURCES:$(SRC)/%.f90=$(OBJ)/%.o)
BINARIES := $(BIN)/day01a $(BIN)/day01b $(BIN)/day02a
TESTS := $(BIN)/day02a_test

all: $(BINARIES)

run: $(BINARIES)
	for BINARY in $(BINARIES); do $${BINARY}; done

runtests: $(TESTS)
	for TEST in $(TESTS); do $${TEST}; done

$(BIN)/day01a: $(OBJ)/day01a.o $(OBJ)/util.o
$(BIN)/day01b: $(OBJ)/day01b.o $(OBJ)/util.o
$(BIN)/day02a: $(OBJ)/day02a_main.o $(OBJ)/day02a.o $(OBJ)/util.o
$(BIN)/day02a_test: $(OBJ)/day02a_test.o $(OBJ)/day02a.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day01a.o: $(OBJ)/util.o
$(OBJ)/day02a.o: $(OBJ)/util.o
$(OBJ)/day02a_main.o: $(OBJ)/day02a.o $(OBJ)/util.o
$(OBJ)/day02a_test.o: $(OBJ)/day02a.o $(OBJ)/util.o $(OBJ)/fruit.o

$(BIN)/%: $(OBJ)/%.o
	$(FC) -o $@ $^

$(OBJ)/%.o: $(SRC)/%.f90
	$(FC) $(FFLAGS) -c -o $@ $<

info:
	@echo 'SOURCES="$(SOURCES)"'
	@echo 'OBJECTS="$(OBJECTS)"'

clean:
	rm -f bin/* obj/*
