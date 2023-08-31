.PHONY: all clean

all: bin/day01a bin/day01b

bin/day01a: src/day01a.f90
	gfortran -o bin/day01a src/day01a.f90

bin/day01b: src/day01b.f90
	gfortran -o bin/day01b src/day01b.f90

clean:
	rm -f bin/*
