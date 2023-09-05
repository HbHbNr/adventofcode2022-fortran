# disable default rules and default variables
MAKEFLAGS += --no-builtin-rules --no-builtin-variables

.PHONY: all run runtests fruitpytests info clean

SRC := src
OBJ := obj
BIN := bin
FC := gfortran
FFLAGS := -J $(OBJ)
SOURCES := $(wildcard $(SRC)/*.f90)
OBJECTS := $(SOURCES:$(SRC)/%.f90=$(OBJ)/%.o)
BINARIES := $(BIN)/day01a $(BIN)/day01b $(BIN)/day02a
TESTS := $(BIN)/day01a_test_driver $(BIN)/day02a_test_driver
FRUITPYTESTS := $(TESTS:$(BIN)/%_test_driver=fruitpy/%.py)

all: $(BINARIES)

run: $(BINARIES)
	for BINARY in $(BINARIES); do $${BINARY}; done

alltests: $(TESTS)

runtests: $(TESTS)
	cd $(BIN) && for TEST in $(TESTS); do ../$${TEST}; done

fruitpytests: $(FRUITPYTESTS)
	for FRUITPYTEST in $(FRUITPYTESTS); do python3 $${FRUITPYTEST}; done

$(BIN)/day01a: $(OBJ)/day01a_main.o $(OBJ)/day01a.o $(OBJ)/util.o
$(BIN)/day01a_test_driver: $(OBJ)/day01a_test_driver.o $(OBJ)/day01a_test.o $(OBJ)/day01a.o $(OBJ)/util.o $(OBJ)/fruit.o

$(BIN)/day01b: $(OBJ)/day01b.o $(OBJ)/util.o

$(BIN)/day02a: $(OBJ)/day02a_main.o $(OBJ)/day02a.o $(OBJ)/util.o
$(BIN)/day02a_test_driver: $(OBJ)/day02a_test_driver.o $(OBJ)/day02a_test.o $(OBJ)/day02a.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day01a_main.o: $(OBJ)/day01a.o $(OBJ)/util.o
$(OBJ)/day01a_test.o: $(OBJ)/day01a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day01a_test_driver.o: $(OBJ)/day01a_test.o $(OBJ)/day01a.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day02a.o: $(OBJ)/util.o

$(OBJ)/day02a_main.o: $(OBJ)/day02a.o $(OBJ)/util.o
$(OBJ)/day02a_test.o: $(OBJ)/day02a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day02a_test_driver.o: $(OBJ)/day02a_test.o $(OBJ)/day02a.o $(OBJ)/util.o $(OBJ)/fruit.o

$(BIN)/%: $(OBJ)/%.o
	$(FC) -o $@ $^

$(OBJ)/%.o: $(SRC)/%.f90
	$(FC) $(FFLAGS) -c -o $@ $<

info:
	@echo 'SOURCES="$(SOURCES)"'
	@echo 'OBJECTS="$(OBJECTS)"'
	@echo 'BINARIES="$(BINARIES)"'
	@echo 'TESTS="$(TESTS)"'
	@echo 'FRUITPYTESTS="$(FRUITPYTESTS)"'

clean:
	rm -f bin/* obj/*
