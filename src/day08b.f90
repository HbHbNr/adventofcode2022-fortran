!> Solution for https://adventofcode.com/2022/day/8 part b
module day08b
    use iso_fortran_env, only : int8, logical_kinds
    use util, only : printioerror, readinputfile_asintarray
    implicit none
    private

    public :: solve

contains

    subroutine checkpath(heightarray, rowfrom, rowto, columnfrom, columnto, totalscore)
        implicit none

        integer(int8), intent(in) :: heightarray(:,:)
        integer, intent(in)       :: rowfrom, rowto, columnfrom, columnto
        integer, intent(inout)    :: totalscore
        integer                   :: refheight, height, distance, row, column, step

        refheight = heightarray(rowfrom, columnfrom)
        if ((rowfrom > rowto) .or. (columnfrom > columnto)) then
            step = -1
        else
            step = 1
        end if
        distance = 0
        ! one dimension of the stride will be one, so one of the loops will
        ! have only a single iteration
        rowloop: do row = rowfrom, rowto, step
            do column = columnfrom, columnto, step
                height = heightarray(row, column)
                if (row == rowfrom .and. column == columnfrom) then
                    ! do not count the reference tree itself as visible
                    cycle
                end if
                distance = distance + 1
                if (height >= refheight) then
                    exit rowloop
                end if
            end do
        end do rowloop
        totalscore = totalscore * distance
    end subroutine

    subroutine checktree(heightarray, visiblearray, refrow, refcolumn)
        implicit none

        integer(int8), intent(in) :: heightarray(:,:)
        integer, intent(inout)    :: visiblearray(:,:)
        integer, intent(in)       :: refrow, refcolumn
        integer                   :: totalscore

        totalscore = 1
        ! look up
        call checkpath(heightarray, refrow, 1, refcolumn, refcolumn, totalscore)
        ! look left
        call checkpath(heightarray, refrow, refrow, refcolumn, 1, totalscore)
        ! look down
        call checkpath(heightarray, refrow, size(heightarray, 1), refcolumn, refcolumn, totalscore)
        ! look right
        call checkpath(heightarray, refrow, refrow, refcolumn, size(heightarray, 2), totalscore)

        visiblearray(refrow, refcolumn) = totalscore
    end subroutine

    integer function solve(filename, linebufferlength)
        implicit none

        character(len=*), intent(in) :: filename
        integer, intent(in)          :: linebufferlength
        integer(int8), allocatable   :: heightarray(:,:)
        integer, allocatable         :: visiblearray(:,:)
        integer                      :: rows, columns, row, column

        ! read file with heights into array
        heightarray = readinputfile_asintarray(filename, linebufferlength)
        rows = size(heightarray,1)
        columns = size(heightarray,2)

        ! create boolean array of the same shape as the height array
        allocate(visiblearray(rows, columns), source=0)

        ! check all trees in all rows and columns to find which tree has the
        ! highest scenic score, i.e. longest visibility lines
        do row = 1, size(heightarray, 1)
            do column = 1, size(heightarray, 2)
                call checktree(heightarray, visiblearray, row, column)
            end do
        end do

        ! solution is the highest scenic score
        solve = maxval(visiblearray)
    end function

end module day08b
