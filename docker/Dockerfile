FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get upgrade && \
    apt-get install --yes make gfortran && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /usr/src/app/adventofcode2022-fortran

RUN mkdir bin obj
COPY fruitpy fruitpy
COPY inputfiles inputfiles
COPY src src
COPY fruitpy.sh Makefile ./

RUN make all tests

CMD ["make", "runall"]
