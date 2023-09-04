.PHONY: all run info clean

SRC := src
OBJ := obj
BIN := bin
FC := gfortran
FFLAGS := -J $(OBJ)
SOURCES := $(wildcard $(SRC)/*.f90)
OBJECTS := $(SOURCES:$(SRC)/%.f90=$(OBJ)/%.o)
BINARIES := $(BIN)/day01a $(BIN)/day01b $(BIN)/day02a

all: $(BINARIES)

run: $(BINARIES)
	for BINARY in $(BINARIES); do $${BINARY}; done

$(BIN)/day01a: $(OBJ)/day01a.o $(OBJ)/util.o
$(BIN)/day01b: $(OBJ)/day01b.o $(OBJ)/util.o
$(BIN)/day02a: $(OBJ)/day02a_main.o $(OBJ)/day02a.o $(OBJ)/util.o

$(OBJ)/day01a.o: $(OBJ)/util.o
$(OBJ)/day02a.o: $(OBJ)/util.o
$(OBJ)/day02a_main.o: $(OBJ)/day02a.o $(OBJ)/util.o

$(BIN)/%: $(OBJ)/%.o
	$(FC) -o $@ $^

$(OBJ)/%.o: $(SRC)/%.f90
	$(FC) $(FFLAGS) -c -o $@ $<

# $(BIN)/day01a: $(SRC)/day01a.f90 $(OBJ)/util.o
# 	gfortran -I $(OBJ) -o $(BIN)/day01a $(SRC)/day01a.f90 $(OBJ)/util.o

# $(BIN)/day01b: $(SRC)/day01b.f90 $(OBJ)/util.o
# 	gfortran -I $(OBJ) -o $(BIN)/day01b $(SRC)/day01b.f90 $(OBJ)/util.o

# $(BIN)/day02a: $(SRC)/day02a_run.f90 $(SRC)/day02a.f90 $(OBJ)/util.o
# 	gfortran -I $(OBJ) -o $(BIN)/day02a $(SRC)/day02a_run.f90 $(SRC)/day02a.f90 $(OBJ)/util.o


# $(OBJ)/day02a.o: $(SRC)/day02a.f90
# 	gfortran -c -J $(OBJ) -o $(OBJ)/day02a.o $(SRC)/day02a.f90

# $(OBJ)/util.o: $(SRC)/util.f90
# 	gfortran -c -J $(OBJ) -o $(OBJ)/util.o $(SRC)/util.f90

info:
	@echo 'SOURCES="$(SOURCES)"'
	@echo 'OBJECTS="$(OBJECTS)"'

clean:
	rm -f bin/* obj/*
