Dockerfile for creating a container from the current repository.
The container will as default run ``make runall`` at startup.

Create the container image; the current working directory needs to be root of
the project:

    docker image build --file docker/Dockerfile --tag adventofcode2022-fortran:latest .

Create and run the container, and remove it afterwards:

    docker container run -it --rm --name adventofcode2022-fortran adventofcode2022-fortran:latest
