.PHONY: all clean

BIN = bin
OBJ = obj
SRC = src

all: $(BIN)/day01a $(BIN)/day01b

$(BIN)/day01a: $(SRC)/day01a.f90 $(OBJ)/util.o
	gfortran -I $(OBJ) -o $(BIN)/day01a $(SRC)/day01a.f90 $(OBJ)/util.o

$(BIN)/day01b: $(SRC)/day01b.f90 $(OBJ)/util.o
	gfortran -I $(OBJ) -o $(BIN)/day01b $(SRC)/day01b.f90 $(OBJ)/util.o

$(OBJ)/util.o: $(SRC)/util.f90
	gfortran -c -J $(OBJ) -o $(OBJ)/util.o $(SRC)/util.f90

clean:
	rm -f $(BIN)/* $(OBJ)/*
