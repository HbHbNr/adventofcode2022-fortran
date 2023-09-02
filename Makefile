.PHONY: all run clean

BIN = bin
OBJ = obj
SRC = src

BINARIES = $(BIN)/day01a $(BIN)/day01b $(BIN)/day02a

all: $(BINARIES)

run: $(BINARIES)
	for BINARY in $(BINARIES); do $${BINARY}; done

$(BIN)/day01a: $(SRC)/day01a.f90 $(OBJ)/util.o
	gfortran -I $(OBJ) -o $(BIN)/day01a $(SRC)/day01a.f90 $(OBJ)/util.o

$(BIN)/day01b: $(SRC)/day01b.f90 $(OBJ)/util.o
	gfortran -I $(OBJ) -o $(BIN)/day01b $(SRC)/day01b.f90 $(OBJ)/util.o

$(BIN)/day02a: $(SRC)/day02a.f90 $(OBJ)/util.o
	gfortran -I $(OBJ) -o $(BIN)/day02a $(SRC)/day02a.f90 $(OBJ)/util.o

$(OBJ)/util.o: $(SRC)/util.f90
	gfortran -c -J $(OBJ) -o $(OBJ)/util.o $(SRC)/util.f90

clean:
	rm -f $(BIN)/* $(OBJ)/*
