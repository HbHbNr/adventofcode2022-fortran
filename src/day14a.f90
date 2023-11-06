!> Solution for https://adventofcode.com/2021/day/14 part a
module day14a
    use util, only : readinputfile_asstringarray
    implicit none
    private

    public :: solve

contains

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        character(len=150)            :: line
        integer                       :: i

        lines = readinputfile_asstringarray(filename, 150)

        do i = 1, size(lines)
            line = lines(i)
            print *, line
        end do

        solve = -1
    end function

end module day14a
