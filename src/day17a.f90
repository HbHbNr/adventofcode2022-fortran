!> Solution for https://adventofcode.com/2022/day/17 part a
module day17a
    use iso_fortran_env, only : error_unit
    use util, only : readinputfile_asline
    implicit none
    private

    character(len=1), parameter :: char_rock = '#'
    character(len=1), parameter :: char_empty = '.'
    integer, parameter :: chamberwidth = 7
    integer, parameter :: maxrocks = 2022
    logical, parameter :: rockdata1(1,4) = reshape([.true., .true., .true., .true.], shape(rockdata1))
    logical, parameter :: rockdata2(3,3) = reshape([.false., .true. , .false., &
                                                    .true. , .true. , .true. ,  &
                                                    .false., .true. , .false.], shape(rockdata2))
    logical, parameter :: rockdata3(3,3) = reshape([.false., .false., .true., &
                                                    .false., .false., .true., &
                                                    .true. , .true. , .true.], shape(rockdata3))
    logical, parameter :: rockdata4(4,1) = reshape([.true., &
                                                    .true., &
                                                    .true., &
                                                    .true.], shape(rockdata4))
    logical, parameter :: rockdata5(2,2) = reshape([.true. , .true., &
                                                    .true. , .true. ], shape(rockdata5))

    type :: Chamber
        private

        integer              :: tower_height
        integer              :: next_rock_type
        logical, allocatable :: space(:,:)
    contains
        procedure :: init      => chamber_init
        procedure :: spawnrock => chamber_spawnrock
        procedure :: print     => chamber_print
    end type Chamber

    type :: Rock
        private

        integer              :: x, y
        integer              :: rock_type
        logical, allocatable :: rockdata(:,:)
    contains
        procedure :: init      => rock_init
        procedure :: pushleft  => rock_pushleft
        procedure :: pushright => rock_pushright
        procedure :: place     => rock_place
        ! procedure :: dropone   => rock_dropone
        procedure :: print     => rock_print
    end type Rock

    public :: solve

contains

    subroutine chamber_init(this)
        implicit none

        class(Chamber), intent(inout) :: this

        ! 5 blocks have total height of 13 space units
        allocate(this%space(ceiling(maxrocks / 5.0) * 13, chamberwidth))
        this%tower_height = 0
        this%next_rock_type = 1
    end subroutine

    function chamber_spawnrock(this) result(therock)
        implicit none

        class(Chamber), intent(inout) :: this
        type(Rock)                    :: therock

        call therock%init(this%next_rock_type, this%tower_height)
        this%next_rock_type = this%next_rock_type + 1
        if (this%next_rock_type > 5) this%next_rock_type = 1
    end function

    subroutine chamber_print(this)
        implicit none

        class(Chamber), intent(inout) :: this
        integer                       :: y

        do y = this%tower_height, lbound(this%space, 1), -1
            print '(7L1)', this%space(y,:)
        end do
    end subroutine

    subroutine rock_init(this, rock_type, tower_height)
        implicit none

        class(Rock), intent(inout)    :: this
        integer, intent(in)           :: rock_type, tower_height

        this%rock_type = rock_type
        this%x = 3
        this%y = tower_height + 4
        select case (this%rock_type)
        case (1)
            this%rockdata = rockdata1
        case (2)
            this%rockdata = rockdata2
        case (3)
            this%rockdata = rockdata3
        case (4)
            this%rockdata = rockdata4
        case (5)
            this%rockdata = rockdata5
        case default
            write (error_unit, *) 'Rock error for getdata(): known rock type', this%rock_type
            stop
        end select
    end subroutine

    subroutine rock_pushleft(this)
        implicit none

        class(Rock), intent(inout) :: this
        logical                    :: fits
        integer                    :: y, x

        if (this%x > 1) then
            fits = .true.
            yloop: do y = lbound(this%rockdata, 1), ubound(this%rockdata, 1)
                do x = lbound(this%rockdata, 2), ubound(this%rockdata, 2)
                    ! todo: check if new place is free for all solid parts of the rock
                    ! if (this%rockdata(y,:)
                end do
            end do yloop
        end if
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

    subroutine rock_place(this, thechamber)
        implicit none

        class(Rock), intent(inout)    :: this
        class(Chamber), intent(inout) :: thechamber
        integer                       :: y, x, spacey, spacex

        do y = lbound(this%rockdata, 1), ubound(this%rockdata, 1)
            do x = lbound(this%rockdata, 2), ubound(this%rockdata, 2)
                spacey = this%y + y - 1
                spacex = this%x + x - 1
                print *, spacey, spacex
                thechamber%space(spacey, spacex) = thechamber%space(spacey, spacex) .or. this%rockdata(y,x)
            end do
        end do
        thechamber%tower_height = max(thechamber%tower_height, this%y + ubound(this%rockdata, 1) - 1)
        call thechamber%print()
        print *
    end subroutine

    subroutine rock_print(this)
        implicit none

        class(Rock), intent(inout) :: this
        integer                    :: y

        print *, this%y, this%x
        do y = ubound(this%rockdata, 1), lbound(this%rockdata, 1), -1
            print *, this%rockdata(y,:)
        end do
    end subroutine

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: line
        type(Chamber)                 :: thechamber
        type(Rock), allocatable       :: therock
        integer                       :: i

        line = readinputfile_asline(filename)
        print *, line
        call thechamber%init()
        call thechamber%print()
        do i = 1, 1
            therock = thechamber%spawnrock()
            call therock%print()
            print *
        end do
        call therock%place(thechamber)
        call thechamber%print()

        solve = -1
    end function

end module day17a
