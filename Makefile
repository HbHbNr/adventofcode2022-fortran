# disable default rules and default variables
MAKEFLAGS += --no-builtin-rules --no-builtin-variables

.PHONY: all runall runbenchmark tests runtests fruitpytests info clean

SRC := src
OBJ := obj
BIN := bin
FC := gfortran
FFLAGS := -J $(OBJ) -Wall -Wextra -fcheck=all -g -std=f2018
SOURCES := $(sort $(wildcard $(SRC)/*.f90))
OBJECTS := $(SOURCES:$(SRC)/%.f90=$(OBJ)/%.o)
BINARIES := $(sort $(patsubst $(SRC)/%_main.f90,$(BIN)/%,$(wildcard $(SRC)/*_main.f90)))
TESTS := $(BINARIES:%=%_test_driver) $(BIN)/class_charstack_test_driver $(BIN)/class_complexlist_test_driver $(BIN)/class_intstack_test_driver $(BIN)/util_test_driver
FRUITPYTESTS := $(TESTS:$(BIN)/%_test_driver=fruitpy/%.py)

all: $(BINARIES)

runall: $(BINARIES)
	for BINARY in $(BINARIES); do $${BINARY}; done

runbenchmark: $(BINARIES)
	for BINARY in $(BINARIES); do /usr/bin/time -f "$${BINARY}: %es" $${BINARY} > /dev/null; done

tests: $(TESTS)

runtests: $(TESTS)
	cd $(BIN) && for TEST in $(TESTS); do ../$${TEST}; done

fruitpytests: $(FRUITPYTESTS)
	@for FRUITPYTEST in $(FRUITPYTESTS); do \
	    echo "******************** $${FRUITPYTEST} ********************"; \
		python3 $${FRUITPYTEST}; \
	done

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

# download fruit.f90 from github.com
$(SRC)/fruit.f90:
	curl -sS https://raw.githubusercontent.com/mortele/FRUIT/3d7f35351a41be2422e4f83364aa1d1c378bc6d2/src/fruit.f90 > $@

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

# shared code dependencies
$(OBJ)/class_charstack.o: $(OBJ)/util.o
$(OBJ)/class_charstack_test.o: $(OBJ)/class_charstack.o $(OBJ)/fruit.o
$(OBJ)/class_charstack_test_driver.o: $(OBJ)/class_charstack_test.o $(OBJ)/class_charstack.o $(OBJ)/fruit.o
$(BIN)/class_charstack_test_driver: $(OBJ)/class_charstack_test_driver.o $(OBJ)/class_charstack_test.o $(OBJ)/class_charstack.o $(OBJ)/fruit.o

$(OBJ)/class_complexlist.o: $(OBJ)/util.o
$(OBJ)/class_complexlist_test.o: $(OBJ)/class_complexlist.o $(OBJ)/fruit.o
$(OBJ)/class_complexlist_test_driver.o: $(OBJ)/class_complexlist_test.o $(OBJ)/class_complexlist.o $(OBJ)/fruit.o
$(BIN)/class_complexlist_test_driver: $(OBJ)/class_complexlist_test_driver.o $(OBJ)/class_complexlist_test.o $(OBJ)/class_complexlist.o $(OBJ)/fruit.o

$(OBJ)/class_intstack.o: $(OBJ)/util.o
$(OBJ)/class_intstack_test.o: $(OBJ)/class_intstack.o $(OBJ)/fruit.o
$(OBJ)/class_intstack_test_driver.o: $(OBJ)/class_intstack_test.o $(OBJ)/class_intstack.o $(OBJ)/fruit.o
$(BIN)/class_intstack_test_driver: $(OBJ)/class_intstack_test_driver.o $(OBJ)/class_intstack_test.o $(OBJ)/class_intstack.o $(OBJ)/fruit.o

$(OBJ)/class_intringbuffer.o: $(OBJ)/util.o
$(OBJ)/class_intringbuffer_test.o: $(OBJ)/class_intringbuffer.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/class_intringbuffer_test_driver.o: $(OBJ)/class_intringbuffer_test.o $(OBJ)/class_intringbuffer.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/class_intringbuffer_test_driver: $(OBJ)/class_intringbuffer_test_driver.o $(OBJ)/class_intringbuffer_test.o $(OBJ)/class_intringbuffer.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/util_test.o: $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/util_test_driver.o: $(OBJ)/util_test.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/util_test_driver: $(OBJ)/util_test_driver.o $(OBJ)/util_test.o $(OBJ)/util.o $(OBJ)/fruit.o

# day solution code dependencies
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

$(OBJ)/day05a.o: $(OBJ)/util.o $(OBJ)/class_charstack.o
$(OBJ)/day05a_main.o: $(OBJ)/day05a.o $(OBJ)/util.o
$(OBJ)/day05a_test.o: $(OBJ)/day05a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day05a_test_driver.o: $(OBJ)/day05a_test.o $(OBJ)/day05a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day05a: $(OBJ)/day05a_main.o $(OBJ)/day05a.o $(OBJ)/util.o $(OBJ)/class_charstack.o
$(BIN)/day05a_test_driver: $(OBJ)/day05a_test_driver.o $(OBJ)/day05a_test.o $(OBJ)/day05a.o $(OBJ)/util.o $(OBJ)/fruit.o $(OBJ)/class_charstack.o

$(OBJ)/day05b.o: $(OBJ)/util.o $(OBJ)/class_charstack.o
$(OBJ)/day05b_main.o: $(OBJ)/day05b.o $(OBJ)/util.o
$(OBJ)/day05b_test.o: $(OBJ)/day05b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day05b_test_driver.o: $(OBJ)/day05b_test.o $(OBJ)/day05b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day05b: $(OBJ)/day05b_main.o $(OBJ)/day05b.o $(OBJ)/util.o $(OBJ)/class_charstack.o
$(BIN)/day05b_test_driver: $(OBJ)/day05b_test_driver.o $(OBJ)/day05b_test.o $(OBJ)/day05b.o $(OBJ)/util.o $(OBJ)/fruit.o $(OBJ)/class_charstack.o

$(OBJ)/day06a.o: $(OBJ)/util.o
$(OBJ)/day06a_main.o: $(OBJ)/day06a.o $(OBJ)/util.o
$(OBJ)/day06a_test.o: $(OBJ)/day06a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day06a_test_driver.o: $(OBJ)/day06a_test.o $(OBJ)/day06a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day06a: $(OBJ)/day06a_main.o $(OBJ)/day06a.o $(OBJ)/util.o
$(BIN)/day06a_test_driver: $(OBJ)/day06a_test_driver.o $(OBJ)/day06a_test.o $(OBJ)/day06a.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day06b.o: $(OBJ)/util.o
$(OBJ)/day06b_main.o: $(OBJ)/day06b.o $(OBJ)/util.o
$(OBJ)/day06b_test.o: $(OBJ)/day06b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day06b_test_driver.o: $(OBJ)/day06b_test.o $(OBJ)/day06b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day06b: $(OBJ)/day06b_main.o $(OBJ)/day06b.o $(OBJ)/util.o
$(BIN)/day06b_test_driver: $(OBJ)/day06b_test_driver.o $(OBJ)/day06b_test.o $(OBJ)/day06b.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day07a.o: $(OBJ)/util.o
$(OBJ)/day07a_main.o: $(OBJ)/day07a.o $(OBJ)/util.o
$(OBJ)/day07a_test.o: $(OBJ)/day07a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day07a_test_driver.o: $(OBJ)/day07a_test.o $(OBJ)/day07a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day07a: $(OBJ)/day07a_main.o $(OBJ)/day07a.o $(OBJ)/util.o
$(BIN)/day07a_test_driver: $(OBJ)/day07a_test_driver.o $(OBJ)/day07a_test.o $(OBJ)/day07a.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day07b.o: $(OBJ)/util.o
$(OBJ)/day07b_main.o: $(OBJ)/day07b.o $(OBJ)/util.o
$(OBJ)/day07b_test.o: $(OBJ)/day07b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day07b_test_driver.o: $(OBJ)/day07b_test.o $(OBJ)/day07b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day07b: $(OBJ)/day07b_main.o $(OBJ)/day07b.o $(OBJ)/util.o
$(BIN)/day07b_test_driver: $(OBJ)/day07b_test_driver.o $(OBJ)/day07b_test.o $(OBJ)/day07b.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day08a.o: $(OBJ)/util.o
$(OBJ)/day08a_main.o: $(OBJ)/day08a.o $(OBJ)/util.o
$(OBJ)/day08a_test.o: $(OBJ)/day08a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day08a_test_driver.o: $(OBJ)/day08a_test.o $(OBJ)/day08a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day08a: $(OBJ)/day08a_main.o $(OBJ)/day08a.o $(OBJ)/util.o
$(BIN)/day08a_test_driver: $(OBJ)/day08a_test_driver.o $(OBJ)/day08a_test.o $(OBJ)/day08a.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day08b.o: $(OBJ)/util.o
$(OBJ)/day08b_main.o: $(OBJ)/day08b.o $(OBJ)/util.o
$(OBJ)/day08b_test.o: $(OBJ)/day08b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day08b_test_driver.o: $(OBJ)/day08b_test.o $(OBJ)/day08b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day08b: $(OBJ)/day08b_main.o $(OBJ)/day08b.o $(OBJ)/util.o
$(BIN)/day08b_test_driver: $(OBJ)/day08b_test_driver.o $(OBJ)/day08b_test.o $(OBJ)/day08b.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day09a.o: $(OBJ)/util.o $(OBJ)/class_complexlist.o
$(OBJ)/day09a_main.o: $(OBJ)/day09a.o $(OBJ)/util.o
$(OBJ)/day09a_test.o: $(OBJ)/day09a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day09a_test_driver.o: $(OBJ)/day09a_test.o $(OBJ)/day09a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day09a: $(OBJ)/day09a_main.o $(OBJ)/day09a.o $(OBJ)/util.o $(OBJ)/class_complexlist.o
$(BIN)/day09a_test_driver: $(OBJ)/day09a_test_driver.o $(OBJ)/day09a_test.o $(OBJ)/day09a.o $(OBJ)/util.o $(OBJ)/fruit.o $(OBJ)/class_complexlist.o

$(OBJ)/day09b.o: $(OBJ)/util.o $(OBJ)/class_complexlist.o
$(OBJ)/day09b_main.o: $(OBJ)/day09b.o $(OBJ)/util.o
$(OBJ)/day09b_test.o: $(OBJ)/day09b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day09b_test_driver.o: $(OBJ)/day09b_test.o $(OBJ)/day09b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day09b: $(OBJ)/day09b_main.o $(OBJ)/day09b.o $(OBJ)/util.o $(OBJ)/class_complexlist.o
$(BIN)/day09b_test_driver: $(OBJ)/day09b_test_driver.o $(OBJ)/day09b_test.o $(OBJ)/day09b.o $(OBJ)/util.o $(OBJ)/fruit.o $(OBJ)/class_complexlist.o

$(OBJ)/day10a.o: $(OBJ)/util.o $(OBJ)/class_intstack.o
$(OBJ)/day10a_main.o: $(OBJ)/day10a.o $(OBJ)/util.o
$(OBJ)/day10a_test.o: $(OBJ)/day10a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day10a_test_driver.o: $(OBJ)/day10a_test.o $(OBJ)/day10a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day10a: $(OBJ)/day10a_main.o $(OBJ)/day10a.o $(OBJ)/util.o $(OBJ)/class_intstack.o
$(BIN)/day10a_test_driver: $(OBJ)/day10a_test_driver.o $(OBJ)/day10a_test.o $(OBJ)/day10a.o $(OBJ)/util.o $(OBJ)/fruit.o $(OBJ)/class_intstack.o

$(OBJ)/day10b.o: $(OBJ)/util.o $(OBJ)/class_intstack.o
$(OBJ)/day10b_main.o: $(OBJ)/day10b.o $(OBJ)/util.o
$(OBJ)/day10b_test.o: $(OBJ)/day10b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day10b_test_driver.o: $(OBJ)/day10b_test.o $(OBJ)/day10b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day10b: $(OBJ)/day10b_main.o $(OBJ)/day10b.o $(OBJ)/util.o $(OBJ)/class_intstack.o
$(BIN)/day10b_test_driver: $(OBJ)/day10b_test_driver.o $(OBJ)/day10b_test.o $(OBJ)/day10b.o $(OBJ)/util.o $(OBJ)/fruit.o $(OBJ)/class_intstack.o

$(OBJ)/day11a.o: $(OBJ)/util.o $(OBJ)/class_intringbuffer.o
$(OBJ)/day11a_main.o: $(OBJ)/day11a.o $(OBJ)/util.o
$(OBJ)/day11a_test.o: $(OBJ)/day11a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day11a_test_driver.o: $(OBJ)/day11a_test.o $(OBJ)/day11a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day11a: $(OBJ)/day11a_main.o $(OBJ)/day11a.o $(OBJ)/util.o $(OBJ)/class_intringbuffer.o
$(BIN)/day11a_test_driver: $(OBJ)/day11a_test_driver.o $(OBJ)/day11a_test.o $(OBJ)/day11a.o $(OBJ)/util.o $(OBJ)/fruit.o $(OBJ)/class_intringbuffer.o

$(OBJ)/day11b.o: $(OBJ)/util.o $(OBJ)/class_intringbuffer.o
$(OBJ)/day11b_main.o: $(OBJ)/day11b.o $(OBJ)/util.o
$(OBJ)/day11b_test.o: $(OBJ)/day11b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day11b_test_driver.o: $(OBJ)/day11b_test.o $(OBJ)/day11b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day11b: $(OBJ)/day11b_main.o $(OBJ)/day11b.o $(OBJ)/util.o $(OBJ)/class_intringbuffer.o
$(BIN)/day11b_test_driver: $(OBJ)/day11b_test_driver.o $(OBJ)/day11b_test.o $(OBJ)/day11b.o $(OBJ)/util.o $(OBJ)/fruit.o $(OBJ)/class_intringbuffer.o

$(OBJ)/day12a.o: $(OBJ)/util.o
$(OBJ)/day12a_main.o: $(OBJ)/day12a.o $(OBJ)/util.o
$(OBJ)/day12a_test.o: $(OBJ)/day12a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day12a_test_driver.o: $(OBJ)/day12a_test.o $(OBJ)/day12a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day12a: $(OBJ)/day12a_main.o $(OBJ)/day12a.o $(OBJ)/util.o
$(BIN)/day12a_test_driver: $(OBJ)/day12a_test_driver.o $(OBJ)/day12a_test.o $(OBJ)/day12a.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day12b.o: $(OBJ)/util.o
$(OBJ)/day12b_main.o: $(OBJ)/day12b.o $(OBJ)/util.o
$(OBJ)/day12b_test.o: $(OBJ)/day12b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day12b_test_driver.o: $(OBJ)/day12b_test.o $(OBJ)/day12b.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day12b: $(OBJ)/day12b_main.o $(OBJ)/day12b.o $(OBJ)/util.o
$(BIN)/day12b_test_driver: $(OBJ)/day12b_test_driver.o $(OBJ)/day12b_test.o $(OBJ)/day12b.o $(OBJ)/util.o $(OBJ)/fruit.o

$(OBJ)/day13a.o: $(OBJ)/util.o $(OBJ)/class_intringbuffer.o
$(OBJ)/day13a_main.o: $(OBJ)/day13a.o $(OBJ)/util.o
$(OBJ)/day13a_test.o: $(OBJ)/day13a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day13a_test_driver.o: $(OBJ)/day13a_test.o $(OBJ)/day13a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day13a: $(OBJ)/day13a_main.o $(OBJ)/day13a.o $(OBJ)/util.o $(OBJ)/class_intringbuffer.o
$(BIN)/day13a_test_driver: $(OBJ)/day13a_test_driver.o $(OBJ)/day13a_test.o $(OBJ)/day13a.o $(OBJ)/util.o $(OBJ)/fruit.o $(OBJ)/class_intringbuffer.o
