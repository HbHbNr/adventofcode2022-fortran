.PHONY: all clean

all: bin/day01a

bin/day01a: src/day01a.f90
	gfortran -o bin/day01a src/day01a.f90

clean:
	rm -f bin/day01a
