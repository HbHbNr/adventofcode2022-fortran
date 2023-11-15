!> Solution for https://adventofcode.com/2021/day/16 part a
module day16a
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 68

    type :: Cave
        private

        character(len=2), allocatable :: rooms(:)
        integer, allocatable          :: tunnels(:,:)
    contains
        procedure :: init => cave_init
    end type Cave

    public :: solve

contains

    subroutine cave_init(this, lines)
        implicit none

        class(Cave), intent(inout) :: this
        character(len=*)           :: lines(:)
        integer                    :: i

        do i = 1, size(lines)
            print *, lines(i)
        end do
    end subroutine

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        type(Cave)                    :: thecave

        lines = readinputfile_asstringarray(filename, maxlinelength)

        call thecave%init(lines)

        solve = -1
    end function

end module day16a
