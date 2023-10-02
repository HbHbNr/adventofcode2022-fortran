!> Solution for https://adventofcode.com/2021/day/9 part a
module day09a
    use iso_fortran_env, only : int8
    use util, only : printioerror, readinputfile_asstringarray
    implicit none
    private

    public :: solve

    type, public :: ComplexList
        private

        complex, allocatable :: values(:)
        integer              :: length
    contains
        procedure :: init     => complexlist_init
        procedure :: add      => complexlist_add
        procedure :: contains => complexlist_contains
    end type ComplexList

contains

    subroutine complexlist_init(this, listsize)
        implicit none

        class(ComplexList), intent(inout) :: this
        integer, intent(in)               :: listsize

        allocate(this%values(listsize))
        this%length = 0
    end subroutine

    subroutine complexlist_add(this, value)
        implicit none

        class(ComplexList), intent(inout) :: this
        complex, intent(in)               :: value

        this%length = this%length + 1
        this%values(this%length) = value
    end subroutine

    function complexlist_contains(this, value) result(contains)
        implicit none

        class(ComplexList), intent(inout) :: this
        complex, intent(in)               :: value
        logical                           :: contains

        contains = any(this%values == value)
    end function

    subroutine analyse_motions(filename, motions, visitlist)
        implicit none

        character(len=*), intent(in)   :: filename
        type(ComplexList), intent(out) :: motions
        type(ComplexList), intent(out) :: visitlist
        character(len=:), allocatable  :: lines(:)
        character(len=5)               :: line
        character(len=1)               :: direction
        integer                        :: totalmotions, repeat, i, r
        complex                        :: motion

        ! read all motions into an array of strings
        lines = readinputfile_asstringarray(filename, len(line))

        ! iterate lines and count total number of motions
        totalmotions = 0
        do i=1, size(lines)
            read(lines(i), *) direction, repeat
            ! print *, direction, repeat
            totalmotions = totalmotions + repeat
        end do
        print *, totalmotions

        ! prepare containers
        call motions%init(totalmotions)
        call visitlist%init(totalmotions)

        ! iterate lines again and create motions
        do i=1, size(lines)
            read(lines(i), *) direction, repeat
            select case (direction)
            case ('U')
                motion = (0, -1)
            case ('D')
                motion = (0, 1)
            case ('L')
                motion = (-1, 0)
            case ('R')
                motion = (1, 0)
            end select
            do r=1, repeat
                call motions%add(motion)
            end do
        end do
    end subroutine

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)   :: filename
        type(ComplexList) :: motions
        type(ComplexList) :: visitlist

        ! analyse series of motions and prepare containers
        call analyse_motions(filename, motions, visitlist)
        ! print *, motions

        ! return number of visited positions
        solve = visitlist%length
    end function

end module day09a
