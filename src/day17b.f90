!> Solution for https://adventofcode.com/2022/day/17 part b
module day17b
    use iso_fortran_env, only : error_unit
    use util, only : readinputfile_asline
    implicit none
    private

    character(len=1), parameter :: char_rock = '#'
    character(len=1), parameter :: char_empty = '.'
    integer, parameter :: chamberwidth = 7
    integer, parameter :: maxrocks = 2022
    integer, parameter :: bufferrocks = 100
    ! rocks are written column by column, from top to bottom and left to right
    logical, parameter :: rockdata1(1,4) = reshape([.true., &
                                                    .true., &
                                                    .true., &
                                                    .true.], shape(rockdata1))
    logical, parameter :: rockdata2(3,3) = reshape([.false., .true. , .false., &
                                                    .true. , .true. , .true. ,  &
                                                    .false., .true. , .false.], shape(rockdata2))
    logical, parameter :: rockdata3(3,3) = reshape([.false., .false., .true., &
                                                    .false., .false., .true., &
                                                    .true. , .true. , .true.], shape(rockdata3))
    logical, parameter :: rockdata4(4,1) = reshape([.true., .true., .true., .true.], shape(rockdata4))
    logical, parameter :: rockdata5(2,2) = reshape([.true. , .true., &
                                                    .true. , .true.], shape(rockdata5))

    type :: Chamber
        private

        integer              :: tower_height
        integer              :: shrinked_tower_height
        integer              :: next_rock_type
        logical, allocatable :: space(:,:)
        logical, allocatable :: movements_left(:)
        integer              :: next_movement_left
    contains
        procedure :: init           => chamber_init
        procedure :: spawnrock      => chamber_spawnrock
        procedure :: shouldpushleft => chamber_shouldpushleft
        procedure :: shrink         => chamber_shrink
        procedure :: allempty       => chamber_allempty
        procedure :: print          => chamber_print
    end type Chamber

    type :: Rock
        private

        integer              :: x, y
        integer              :: rock_type
        logical, allocatable :: rockdata(:,:)
    contains
        procedure :: init      => rock_init
        procedure :: move      => rock_move
        procedure :: place     => rock_place
        procedure :: print     => rock_print
    end type Rock

    public :: solve

contains

    subroutine chamber_init(this, movements)
        implicit none

        class(Chamber), intent(inout) :: this
        character(len=*), intent(in)  :: movements
        integer                       :: i

        ! 5 blocks have total height of 13 space units
        allocate(this%space(ceiling(bufferrocks / 5.0) * 13, chamberwidth), source=.false.)
        this%tower_height = 0
        this%shrinked_tower_height = 0
        this%next_rock_type = 1

        ! translate string of '<' and '>' into .true. and .false.
        allocate(this%movements_left(len(movements)))
        do i = 1, len(movements)
            this%movements_left(i) = movements(i:i) == '<'
        end do
        ! print *, this%movements_left
        this%next_movement_left = 1
    end subroutine

    function chamber_spawnrock(this) result(therock)
        implicit none

        class(Chamber), intent(inout) :: this
        type(Rock)                    :: therock

        call therock%init(this%next_rock_type, this%tower_height)
        this%next_rock_type = this%next_rock_type + 1
        if (this%next_rock_type > 5) this%next_rock_type = 1
    end function

    function chamber_shouldpushleft(this) result(nextleft)
        implicit none

        class(Chamber), intent(inout) :: this
        logical                       :: nextleft

        nextleft = this%movements_left(this%next_movement_left)
        this%next_movement_left = this%next_movement_left + 1
        if (this%next_movement_left > size(this%movements_left)) this%next_movement_left = 1
    end function

    function chamber_allempty(this, from_y, to_y, x) result(allempty)
        implicit none

        class(Chamber), intent(in) :: this
        integer, intent(in)        :: from_y, to_y, x
        logical                    :: allempty
        integer                    :: y

        allempty = .true.
        do y = from_y, to_y
            if (this%space(y, x)) then
                allempty = .false.
                exit
            end if
        end do
    end function

    subroutine chamber_print(this)
        implicit none

        class(Chamber), intent(inout) :: this
        integer                       :: y, x

        do y = this%tower_height+5, lbound(this%space, 1), -1
            write (*, '(A1)', advance='no') '|'
            do x = lbound(this%space, 2), ubound(this%space, 2)
                if (this%space(y,x)) then
                    write (*, '(A1)', advance='no') char_rock
                else
                    write (*, '(A1)', advance='no') char_empty
                end if
            end do
            write (*, '(A1)') '|'
        end do
        write (*, '(A)') '+-------+'
    end subroutine

    subroutine chamber_shrink(this, place_y)
        implicit none

        class(Chamber), intent(inout) :: this
        integer, intent(in)           :: place_y
        integer                       :: x, remove_tower_height
        logical                       :: slotfound
        integer, parameter            :: inspect_layers = 3
        integer                       :: calls = 0

        if (place_y > inspect_layers + 1) then
            ! print *, 'calls=', calls
            ! call this%print()
            ! print *, 'place_y=', place_y
            slotfound = .false.
            do x = 1, chamberwidth
                ! print *, this%space(place_y-inspect_layers:place_y+inspect_layers, x)
                if (this%allempty(place_y-inspect_layers, place_y+inspect_layers, x)) then
                    ! there is a vertical slot through the four layers, so they are still traversable
                    slotfound = .true.
                    ! print *, 'slot found in x=', x
                    exit
                end if
            end do
            if (slotfound .eqv. .false.) then
                ! there is no vertical slot through the four layers, so they are not traversable
                ! -> count the lower layers and remove them
                ! call this%print()
                ! print *, 'shrinking'
                ! print *, this%tower_height, this%shrinked_tower_height
                remove_tower_height = place_y - inspect_layers - 1
                do x = 1, chamberwidth
                    this%space(1:this%tower_height-remove_tower_height, x) = this%space(place_y-inspect_layers:this%tower_height, x)  ! move down top layers
                    this%space(this%tower_height-remove_tower_height+1:this%tower_height, x) = .false.
                end do
                this%tower_height = this%tower_height - remove_tower_height
                this%shrinked_tower_height = this%shrinked_tower_height + remove_tower_height
                ! print *, this%tower_height, this%shrinked_tower_height
                ! call this%print()
            end if
            ! call this%print()
            ! print *
            ! if (calls < 3000) then
            !     calls = calls + 1
            ! else
            !     stop
            ! end if
        end if
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

    function rock_move(this, thechamber, movex, movey) result(fits)
        implicit none

        class(Rock), intent(inout)    :: this
        class(Chamber), intent(inout) :: thechamber
        integer, intent(in)           :: movex, movey
        logical                       :: fits
        integer                       :: newx, newy, y, x, spacey, spacex

        newy = this%y + movey
        newx = this%x + movex
        fits = .false.
        if (newy < 1) then
            ! rock too far down, so don't move
            return
        else if (newx < 1) then
            ! rock too far to the left, so don't move
            return
        else if (newx + ubound(this%rockdata, 2) - 1 > chamberwidth) then
            ! rock too far to the right, so don't move
            return
        end if
        do y = lbound(this%rockdata, 1), ubound(this%rockdata, 1)
            do x = lbound(this%rockdata, 2), ubound(this%rockdata, 2)
                if (this%rockdata(y,x) .eqv. .false.) then
                    ! rock has empty space at that position, so we do not care about the chamber
                    cycle
                end if
                spacey = newy + ubound(this%rockdata, 1) - y
                spacex = newx + x - 1
                ! print *, 'space:', movex, movey, newx, newy, spacex, spacey, this%rock_type
                if (thechamber%space(spacey, spacex)) then
                    ! chamber is oocupied at the position, so don't move
                    return
                end if
            end do
        end do
        this%x = newx
        this%y = newy
        fits = .true.
    end function

    subroutine rock_place(this, thechamber)
        implicit none

        class(Rock), intent(inout)    :: this
        class(Chamber), intent(inout) :: thechamber
        integer                       :: y, x, spacey, spacex

        do y = lbound(this%rockdata, 1), ubound(this%rockdata, 1)
            do x = lbound(this%rockdata, 2), ubound(this%rockdata, 2)
                spacey = this%y + ubound(this%rockdata, 1) - y
                spacex = this%x + x - 1
                thechamber%space(spacey, spacex) = thechamber%space(spacey, spacex) .or. this%rockdata(y,x)
            end do
        end do
        thechamber%tower_height = max(thechamber%tower_height, this%y + ubound(this%rockdata, 1) - 1)
    end subroutine

    subroutine rock_print(this)
        implicit none

        class(Rock), intent(inout) :: this
        integer                    :: y

        print *, 'pos:', this%x, this%y
        do y = lbound(this%rockdata, 1), ubound(this%rockdata, 1)
            print *, this%rockdata(y,:)
        end do
    end subroutine

    function simulate(movements) result(tower_height)
        implicit none

        character(len=*), intent(in) :: movements
        integer                      :: tower_height
        type(Chamber)                :: thechamber
        type(Rock), allocatable      :: therock
        integer                      :: i
        logical                      :: fits

        call thechamber%init(movements)

        do i = 1, maxrocks
            therock = thechamber%spawnrock()
            ! call therock%print()
            fits = .true.
            do while(fits)
                if (thechamber%shouldpushleft()) then
                    fits = therock%move(thechamber, -1, 0)
                else
                    fits = therock%move(thechamber, 1, 0)
                end if
                fits = therock%move(thechamber, 0, -1)
            end do
            call therock%place(thechamber)
            call thechamber%shrink(therock%y)
            ! call thechamber%print()
            ! print *
        end do

        ! call thechamber%print()
        tower_height = thechamber%tower_height + thechamber%shrinked_tower_height
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: line

        line = readinputfile_asline(filename)
        ! print *, line

        solve = simulate(line)
    end function

end module day17b
