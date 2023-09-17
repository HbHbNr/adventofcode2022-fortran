# disable default rules and default variables
MAKEFLAGS += --no-builtin-rules --no-builtin-variables

.PHONY: all runall tests runtests fruitpytests info clean

SRC := src
OBJ := obj
BIN := bin
FC := gfortran
FFLAGS := -J $(OBJ) -Wall -Wextra -fcheck=all -g -std=f2018
SOURCES := $(sort $(wildcard $(SRC)/*.f90))
OBJECTS := $(SOURCES:$(SRC)/%.f90=$(OBJ)/%.o)
BINARIES := $(sort $(patsubst $(SRC)/%_main.f90,$(BIN)/%,$(wildcard $(SRC)/*_main.f90)))
TESTS := $(BINARIES:%=%_test_driver) $(BIN)/class_charstack_test_driver
FRUITPYTESTS := $(TESTS:$(BIN)/%_test_driver=fruitpy/%.py)

all: $(BINARIES)

runall: $(BINARIES)
	for BINARY in $(BINARIES); do $${BINARY}; done

tests: $(TESTS)

runtests: $(TESTS)
	cd $(BIN) && for TEST in $(TESTS); do ../$${TEST}; done

fruitpytests: $(FRUITPYTESTS)
	for FRUITPYTEST in $(FRUITPYTESTS); do python3 $${FRUITPYTEST}; done

info:
	@echo 'SOURCES="$(SOURCES)"'
	@echo 'OBJECTS="$(OBJECTS)"'
	@echo 'BINARIES="$(BINARIES)"'
	@echo 'TESTS="$(TESTS)"'
	@echo 'FRUITPYTESTS="$(FRUITPYTESTS)"'

clean:
	rm -f bin/* obj/*

# pattern rules
$(BIN)/%: $(OBJ)/%.o
	$(FC) -o $@ $^

$(OBJ)/%.o: $(SRC)/%.f90
	$(FC) $(FFLAGS) -c -o $@ $<

# source code cloning rules
# - using day01a as a template for day##a
# - using day##a as a template for day##b
$(SRC)/day01a_main.f90 $(SRC)/day01a_test.f90 $(SRC)/day01a.f90 fruitpy/day01a.py:
	# prevent circular dependency

$(SRC)/day%a_main.f90: | $(SRC)/day01a_main.f90
	sed -e 's/day01a/day$(*)a/g' \
	    -e 's#inputfiles/day01#inputfiles/day$(*)#' \
		-e "s/'01a'/'$(*)a'/" \
		$(SRC)/day01a_main.f90 > $@

$(SRC)/day%b_main.f90: | $(SRC)/day%a_main.f90
	sed -e 's/day$(*)a/day$(*)b/g' \
		-e "s/'$(*)a'/'$(*)b'/" \
		$(SRC)/day$(*)a_main.f90 > $@

$(SRC)/day%a_test.f90: | $(SRC)/day01a_test.f90
	sed -e 's/day01a/day$(*)a/g' \
	    -e 's#inputfiles/day01#inputfiles/day$(*)#' \
		-e "s/'01a'/'$(*)a'/" \
		$(SRC)/day01a_test.f90 > $@

$(SRC)/day%b_test.f90: | $(SRC)/day%a_test.f90
	sed -e 's/day$(*)a/day$(*)b/g' \
		-e "s/'$(*)a'/'$(*)b'/" \
		$(SRC)/day$(*)a_test.f90 > $@

$(SRC)/day%a.f90: | $(SRC)/day01a.f90
	sed -e 's/day01a/day$(*)a/g' \
		-e "s#day/1 part a#day/$(*:0%=%) part a#" \
		$(SRC)/day01a.f90 > $@

$(SRC)/day%b.f90: | $(SRC)/day%a.f90
	sed -e 's/day$(*)a/day$(*)b/g' \
		-e "s#day/$(*:0%=%) part a#day/$(*:0%=%) part b#" \
		$(SRC)/day$(*)a.f90 > $@

fruitpy/day%a.py: | fruitpy/day01a.py
	sed -e 's/day01a/day$(*)a/g' \
		fruitpy/day01a.py > $@

fruitpy/day%b.py: | fruitpy/day%a.py
	sed -e 's/day$(*)a/day$(*)b/g' \
		fruitpy/day$(*)a.py > $@

# dependencies
$(OBJ)/class_charstack.o: $(OBJ)/util.o
$(OBJ)/class_charstack_test.o: $(OBJ)/class_charstack.o $(OBJ)/fruit.o
$(OBJ)/class_charstack_test_driver.o: $(OBJ)/class_charstack_test.o $(OBJ)/class_charstack.o $(OBJ)/fruit.o
$(BIN)/class_charstack_test_driver: $(OBJ)/class_charstack_test_driver.o $(OBJ)/class_charstack_test.o $(OBJ)/class_charstack.o $(OBJ)/fruit.o

$(OBJ)/day01a.o: $(OBJ)/util.o
$(OBJ)/day01a_main.o: $(OBJ)/day01a.o $(OBJ)/util.o
$(OBJ)/day01a_test.o: $(OBJ)/day01a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day01a_test_driver.o: $(OBJ)/day01a_test.o $(OBJ)/day01a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day01a: $(OBJ)/day01a_main.o $(OBJ)/day01a.o $(OBJ)/util.o
$(BIN)/day01a_test_driver: $(OBJ)/day01a_test_driver.o $(OBJ)/day01a_test.o $(OBJ)/day01a.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day01b.o: $(OBJ)/util.o
$(OBJ)/day01b_main.o: $(OBJ)/day01b.o $(OBJ)/util.o
$(OBJ)/day01b_test.o: $(OBJ)/day01b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day01b_test_driver.o: $(OBJ)/day01b_test.o $(OBJ)/day01b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day01b: $(OBJ)/day01b_main.o $(OBJ)/day01b.o $(OBJ)/util.o
$(BIN)/day01b_test_driver: $(OBJ)/day01b_test_driver.o $(OBJ)/day01b_test.o $(OBJ)/day01b.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day02a.o: $(OBJ)/util.o
$(OBJ)/day02a_main.o: $(OBJ)/day02a.o $(OBJ)/util.o
$(OBJ)/day02a_test.o: $(OBJ)/day02a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day02a_test_driver.o: $(OBJ)/day02a_test.o $(OBJ)/day02a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day02a: $(OBJ)/day02a_main.o $(OBJ)/day02a.o $(OBJ)/util.o
$(BIN)/day02a_test_driver: $(OBJ)/day02a_test_driver.o $(OBJ)/day02a_test.o $(OBJ)/day02a.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day02b.o: $(OBJ)/util.o
$(OBJ)/day02b_main.o: $(OBJ)/day02b.o $(OBJ)/util.o
$(OBJ)/day02b_test.o: $(OBJ)/day02b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day02b_test_driver.o: $(OBJ)/day02b_test.o $(OBJ)/day02b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day02b: $(OBJ)/day02b_main.o $(OBJ)/day02b.o $(OBJ)/util.o
$(BIN)/day02b_test_driver: $(OBJ)/day02b_test_driver.o $(OBJ)/day02b_test.o $(OBJ)/day02b.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day03a.o: $(OBJ)/util.o
$(OBJ)/day03a_main.o: $(OBJ)/day03a.o $(OBJ)/util.o
$(OBJ)/day03a_test.o: $(OBJ)/day03a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day03a_test_driver.o: $(OBJ)/day03a_test.o $(OBJ)/day03a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day03a: $(OBJ)/day03a_main.o $(OBJ)/day03a.o $(OBJ)/util.o
$(BIN)/day03a_test_driver: $(OBJ)/day03a_test_driver.o $(OBJ)/day03a_test.o $(OBJ)/day03a.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day03b.o: $(OBJ)/util.o
$(OBJ)/day03b_main.o: $(OBJ)/day03b.o $(OBJ)/util.o
$(OBJ)/day03b_test.o: $(OBJ)/day03b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day03b_test_driver.o: $(OBJ)/day03b_test.o $(OBJ)/day03b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day03b: $(OBJ)/day03b_main.o $(OBJ)/day03b.o $(OBJ)/util.o
$(BIN)/day03b_test_driver: $(OBJ)/day03b_test_driver.o $(OBJ)/day03b_test.o $(OBJ)/day03b.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day04a.o: $(OBJ)/util.o
$(OBJ)/day04a_main.o: $(OBJ)/day04a.o $(OBJ)/util.o
$(OBJ)/day04a_test.o: $(OBJ)/day04a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day04a_test_driver.o: $(OBJ)/day04a_test.o $(OBJ)/day04a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day04a: $(OBJ)/day04a_main.o $(OBJ)/day04a.o $(OBJ)/util.o
$(BIN)/day04a_test_driver: $(OBJ)/day04a_test_driver.o $(OBJ)/day04a_test.o $(OBJ)/day04a.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day04b.o: $(OBJ)/util.o
$(OBJ)/day04b_main.o: $(OBJ)/day04b.o $(OBJ)/util.o
$(OBJ)/day04b_test.o: $(OBJ)/day04b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day04b_test_driver.o: $(OBJ)/day04b_test.o $(OBJ)/day04b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day04b: $(OBJ)/day04b_main.o $(OBJ)/day04b.o $(OBJ)/util.o
$(BIN)/day04b_test_driver: $(OBJ)/day04b_test_driver.o $(OBJ)/day04b_test.o $(OBJ)/day04b.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day05a.o: $(OBJ)/util.o
$(OBJ)/day05a_main.o: $(OBJ)/day05a.o $(OBJ)/util.o
$(OBJ)/day05a_test.o: $(OBJ)/day05a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day05a_test_driver.o: $(OBJ)/day05a_test.o $(OBJ)/day05a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day05a: $(OBJ)/day05a_main.o $(OBJ)/day05a.o $(OBJ)/util.o
$(BIN)/day05a_test_driver: $(OBJ)/day05a_test_driver.o $(OBJ)/day05a_test.o $(OBJ)/day05a.o $(OBJ)/util.o $(OBJ)/fruit.o
