!> Solution for https://adventofcode.com/2022/day/17 part a
module day17a
    use util, only : readinputfile_asline
    implicit none
    private

    public :: solve

    type :: Chamber
        private

        integer :: tower_height
        integer :: next_rock_type
    contains
        procedure :: init      => chamber_init
        procedure :: spawnrock => chamber_spawnrock
    end type Chamber

    type :: Rock
        private

        integer :: x, y
        integer :: rock_type
    contains
        procedure :: init      => rock_init
        procedure :: pushleft  => rock_pushleft
        procedure :: pushright => rock_pushright
        ! procedure :: dropone   => rock_dropone
    end type Rock

contains

    subroutine chamber_init(this, maxrocks)
        implicit none

        class(Chamber), intent(inout) :: this
        integer, intent(in)           :: maxrocks

        this%tower_height = 0
        this%next_rock_type = 0
    end subroutine

    function chamber_spawnrock(this) result(therock)
        implicit none

        class(Chamber), intent(inout) :: this
        type(Rock)                    :: therock

        call therock%init(this%next_rock_type, this%tower_height)
        this%next_rock_type = this%next_rock_type + 1
        if (this%next_rock_type > 5) this%next_rock_type = 1
    end function

    subroutine rock_init(this, rock_type, tower_height)
        implicit none

        class(Rock), intent(inout) :: this
        integer, intent(in)        :: rock_type, tower_height

        this%rock_type = rock_type
        this%x = 2
        this%y = tower_height + 3
    end subroutine

    subroutine rock_pushleft(this)
        implicit none

        class(Rock), intent(inout) :: this

        if (this%x > 1) this%x = this%x - 1
    end subroutine

    subroutine rock_pushright(this)
        implicit none

        class(Rock), intent(inout) :: this

        select case (this%rock_type)
        case (1)
            ! if (this%x > 1) this%x = this%x + 1
            ! check if push to right works
        case (2)
        end select
    end subroutine

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: line

        line = readinputfile_asline(filename)
        print *, line

        solve = -1
    end function

end module day17a
