!> Solution for https://adventofcode.com/2021/day/7 part a
module day07a
    use util, only : printioerror, readinputfile_asarray
    implicit none
    private

    public :: solve

contains

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)    :: filename
        integer, parameter              :: linebufferlength = 32
        character(len=linebufferlength), allocatable :: lines(:)
        character(len=linebufferlength) :: line
        integer                         :: i

        ! read whole file into an array of strings
        print *, 'open file ', filename, ' for reading'
        lines = readinputfile_asarray(filename, linebufferlength)
        do i = 1, size(lines)
            line = lines(i)
            print *, '"', line, '"'
        end do

        solve = size(lines)
    end function

end module day07a
