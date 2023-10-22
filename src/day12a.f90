!> Solution for https://adventofcode.com/2021/day/12 part a
module day12a
    use iso_fortran_env, only : logical_kinds
    use util, only : readinputfile_asstringarray
    implicit none
    private

    type :: Position
        integer :: row, col
    end type

    public :: solve

contains

    subroutine find_startposendpos(matrix, startpos, endpos)
        implicit none

        character(len=*), intent(in)  :: matrix(:)
        type(Position), intent(out)   :: startpos, endpos
        integer                       :: col, row
        ! character(len=len(matrix(1))) :: tmpstring

        do row = 1, size(matrix)
            col = index(matrix(row), 'S')
            if (col > 0) then
                startpos%row = col
                startpos%col = row
            end if
            col = index(matrix(row), 'E')
            if (col > 0) then
                endpos%row = col
                endpos%col = row
            end if
        end do
    end subroutine

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: matrix(:)
        logical(kind=1), allocatable  :: closelist(:,:)
        type(Position)                :: startpos, endpos

        ! read matrix from file as an array of strings
        matrix = readinputfile_asstringarray(filename, 200)
        ! create 2-dimensional array of logicals of the same size
        allocate(closelist(size(matrix),len(matrix(1))))

        call find_startposendpos(matrix, startpos, endpos)
        print *, startpos
        print *, endpos

        ! return length of shortest path
        solve = -1
    end function

end module day12a
