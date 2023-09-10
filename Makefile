# disable default rules and default variables
MAKEFLAGS += --no-builtin-rules --no-builtin-variables

.PHONY: all runall tests runtests fruitpytests info clean

SRC := src
OBJ := obj
BIN := bin
FC := gfortran
FFLAGS := -J $(OBJ)
SOURCES := $(sort $(wildcard $(SRC)/*.f90))
OBJECTS := $(SOURCES:$(SRC)/%.f90=$(OBJ)/%.o)
BINARIES := $(sort $(patsubst $(SRC)/%_main.f90,$(BIN)/%,$(wildcard $(SRC)/*_main.f90)))
TESTS := $(sort $(BINARIES:%=%_test_driver))
FRUITPYTESTS := $(TESTS:$(BIN)/%_test_driver=fruitpy/%.py)

all: $(BINARIES)

runall: $(BINARIES)
	for BINARY in $(BINARIES); do $${BINARY}; done

tests: $(TESTS)

runtests: $(TESTS)
	cd $(BIN) && for TEST in $(TESTS); do ../$${TEST}; done

fruitpytests: $(FRUITPYTESTS)
	for FRUITPYTEST in $(FRUITPYTESTS); do python3 $${FRUITPYTEST}; done

$(OBJ)/day01a_main.o: $(OBJ)/day01a.o $(OBJ)/util.o
$(OBJ)/day01a_test.o: $(OBJ)/day01a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(OBJ)/day01a_test_driver.o: $(OBJ)/day01a_test.o $(OBJ)/day01a.o $(OBJ)/util.o $(OBJ)/fruit.o
$(BIN)/day01a: $(OBJ)/day01a_main.o $(OBJ)/day01a.o $(OBJ)/util.o
$(BIN)/day01a_test_driver: $(OBJ)/day01a_test_driver.o $(OBJ)/day01a_test.o $(OBJ)/day01a.o $(OBJ)/util.o $(OBJ)/fruit.o

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

$(BIN)/%: $(OBJ)/%.o
	$(FC) -o $@ $^

$(OBJ)/%.o: $(SRC)/%.f90
	$(FC) $(FFLAGS) -c -o $@ $<

# source code creating rules - using day01 a as a template

$(SRC)/day01a_main.f90 $(SRC)/day01a_test.f90 fruitpy/day01a.py:
	# prevent circular dependency

$(SRC)/day%_main.f90: $(SRC)/day01a_main.f90
	day0number=$$(echo day$(*) | head -c 5); \
	sed -e 's/day01a/day$(*)/g' \
	    -e 's#inputfiles/day01#inputfiles/'$${day0number}'#' \
		-e "s/'01a'/'$(*)'/" \
		$(SRC)/day01a_main.f90 > $@

$(SRC)/day%_test.f90: $(SRC)/day01a_test.f90
	day0number=$$(echo day$(*) | head -c 5); \
	daynumber=$$(echo $$day0number | sed -E -e 's/^0//'); \
	sed -e 's/day01a/day$(*)/g' \
	    -e 's#inputfiles/day01#inputfiles/'$${day0number}'#' \
		-e "s/'01a'/'$(*)'/" \
		$(SRC)/day01a_test.f90 > $@

fruitpy/day%.py: fruitpy/day01a.py
	day0number=$$(echo day$(*) | head -c 5); \
	sed -e 's/day01a/day$(*)/g' \
		fruitpy/day01a.py > $@

info:
	@echo 'SOURCES="$(SOURCES)"'
	@echo 'OBJECTS="$(OBJECTS)"'
	@echo 'BINARIES="$(BINARIES)"'
	@echo 'TESTS="$(TESTS)"'
	@echo 'FRUITPYTESTS="$(FRUITPYTESTS)"'

clean:
	rm -f bin/* obj/*
