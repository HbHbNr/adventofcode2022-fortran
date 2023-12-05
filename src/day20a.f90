!> Solution for https://adventofcode.com/2022/day/20 part a
module day20a
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 5

    public :: solve

contains

    function find_coordinates(numbers) result(coordinates)
        implicit none

        integer, intent(in)  :: numbers(:)
        integer              :: coordinates
        integer, allocatable :: buffer(:)
        integer              :: i, buffersize, number, from_pos, to_pos

        ! create clone of original numbers
        buffersize = size(numbers)
        allocate(buffer(buffersize))
        buffer(:) = numbers(:)

        ! find numbers in buffer and move them around
        print '(7I4)', buffer
        do i = 1, size(numbers)
            number = numbers(i)
            from_pos = findloc(buffer, number, 1)
            to_pos = from_pos + number

            ! ! move one more position if number is negativ - to match examples
            ! if (number < 0) to_pos = to_pos - 1

            ! move to_position into buffer range if needed
            if (to_pos > buffersize) then
                do while (to_pos > buffersize)
                    to_pos = to_pos - buffersize
                end do
                to_pos = to_pos + 1
            end if
            if (to_pos < 1) then
                do while (to_pos < 1)
                    to_pos = to_pos + buffersize
                end do
                to_pos = to_pos - 1
            end if

            ! next iteration if number does not need to be moved
            if (from_pos == to_pos) cycle

            if (from_pos < to_pos) then
                buffer(from_pos:to_pos-1) = buffer(from_pos+1:to_pos)
            else
                ! frompos > topos
                buffer(to_pos+1:from_pos) = buffer(to_pos:from_pos-1)
            end if
            buffer(to_pos) = number
            print '(7I4)', buffer
        end do

        coordinates = -1
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer                       :: i, coordinates
        integer, allocatable          :: numbers(:)

        lines = readinputfile_asstringarray(filename, maxlinelength)
        allocate(numbers(size(lines)))

        ! do i = 1, size(lines)
        !     read (lines(i), *) numbers(i)
        ! end do
        ! coordinates = find_coordinates(numbers)

        ! solve = coordinates
        solve = -1
    end function

end module day20a
