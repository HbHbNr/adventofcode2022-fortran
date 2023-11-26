!> Solution for https://adventofcode.com/2022/day/17 part a
module day17a
    use util, only : readinputfile_asline
    implicit none
    private

    public :: solve

contains

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: line

        line = readinputfile_asline(filename)
        print *, line

        solve = -1
    end function

end module day17a
