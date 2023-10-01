!> Solution for https://adventofcode.com/2021/day/8 part a
module day08a
    use iso_fortran_env, only : int8, logical_kinds
    use util, only : printioerror, readinputfile_asintarray
    implicit none
    private

    public :: solve

contains

    subroutine checkstride(heightarray, visiblearray)
        implicit none

        integer(int8), intent(in)      :: heightarray(:,:)
        logical(kind=1), intent(inout) :: visiblearray(:,:)
        integer                        :: height, currenthighest, row, column

        ! one dimension of the stride will be one, so one of the loops will
        ! have only a single iteration
        currenthighest = -1
        do row = 1, size(heightarray, 1)
            do column = 1, size(heightarray, 2)
                height = heightarray(row, column)
                if (height > currenthighest) then
                    currenthighest = height
                    visiblearray(row, column) = .true.
                end if
            end do
        end do
    end subroutine

    integer function solve(filename, linebufferlength)
        implicit none

        character(len=*), intent(in) :: filename
        integer, intent(in)          :: linebufferlength
        integer(int8), allocatable   :: heightarray(:,:)
        logical(kind=1), allocatable :: visiblearray(:,:)
        integer                      :: rows, columns, row

        ! read file with heights into array
        heightarray = readinputfile_asintarray(filename, linebufferlength)
        rows = size(heightarray,1)
        columns = size(heightarray,2)

        ! create boolean array of the same shape as the height array
        allocate(visiblearray(rows, columns), source=logical(.false., kind=1))

        ! check all row and columns in both directions to find which trees are visible
        ! from the outside
        do row = 1, rows
            call checkstride(heightarray(row:row,:), visiblearray(row:row,:))
            call checkstride(heightarray(row:row,columns:1:-1), visiblearray(row:row,columns:1:-1))
        end do
        do row = 1, columns
            call checkstride(heightarray(:,row:row), visiblearray(:,row:row))
            call checkstride(heightarray(rows:1:-1,row:row), visiblearray(rows:1:-1,row:row))
        end do

        solve = count(visiblearray)
    end function

end module day08a
