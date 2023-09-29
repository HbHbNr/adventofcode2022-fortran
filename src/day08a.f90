!> Solution for https://adventofcode.com/2021/day/8 part a
module day08a
    use iso_fortran_env, only : int8, logical_kinds
    use util, only : printioerror, readinputfile_asintarray
    implicit none
    private

    public :: solve

contains

    subroutine checkrow(heightarray, visiblearray, rownumber, reversed)
        implicit none

        integer(int8), intent(in)      :: heightarray(:,:)
        logical(kind=1), intent(inout) :: visiblearray(:,:)
        integer, intent(in)            :: rownumber
        logical, intent(in)            :: reversed
        integer                        :: height, currenthighest
        integer                        :: row, column, start, end, step

        row = rownumber
        currenthighest = 0
        if (.not. reversed) then
            start = 1
            end = size(heightarray, 2)
            step = 1
        else
            start = size(heightarray, 2)
            end = 1
            step = -1
        end if
        do column = start, end, step
            height = heightarray(row, column)
            if (height > currenthighest) then
                currenthighest = height
                visiblearray(row, column) = .true.
            end if
        end do
    end subroutine

    integer function solve(filename, linebufferlength)
        implicit none

        character(len=*), intent(in) :: filename
        integer, intent(in)          :: linebufferlength
        integer(int8), allocatable   :: heightarray(:,:)
        logical(kind=1), allocatable :: visiblearray(:,:)
        integer                      :: rownumber

        ! print *, logical_kinds
        heightarray = readinputfile_asintarray(filename, linebufferlength)
        do rownumber = 1, size(heightarray, 1)
            ! print all rows
            print *, heightarray(rownumber, :)
        end do
        allocate(visiblearray(size(heightarray,1), size(heightarray,2)), source=logical(.false., kind=1))
        do rownumber = 1, size(visiblearray, 1)
            ! print all rows
            print *, visiblearray(rownumber, :)
        end do
        print *
        do rownumber = 1, size(visiblearray, 1)
            call checkrow(heightarray, visiblearray, rownumber, .false.)
            call checkrow(heightarray, visiblearray, rownumber, .true.)
        end do
        do rownumber = 1, size(visiblearray, 1)
            ! print all rows
            print *, visiblearray(rownumber, :)
        end do
        solve = -1
    end function

end module day08a
