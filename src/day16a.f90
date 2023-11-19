!> Solution for https://adventofcode.com/2021/day/16 part a
module day16a
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 68
    integer, parameter :: maxtunnels = 5
    integer, parameter :: maxminutes = 30

    type :: Cave
        private

        character(len=2), allocatable :: valves(:)
        integer, allocatable          :: flowrates(:)
        integer, allocatable          :: tunnelcounts(:)
        integer, allocatable          :: tunnels(:,:)
        integer                       :: workingvalves
    contains
        procedure :: init => cave_init
    end type Cave

    public :: solve

contains

    subroutine cave_init(this, lines)
        implicit none

        class(Cave), intent(inout)    :: this
        character(len=*)              :: lines(:)
        character(len=:), allocatable :: line, tunnels
        character(len=2)              :: tunnel
        integer                       :: linelength, i, j, valvecount, tunnelcount

        linelength = len(lines(1))
        allocate(character(len=linelength) :: line)
        allocate(character(len=linelength) :: tunnels)
        valvecount = size(lines)
        allocate(this%valves(valvecount))
        allocate(this%flowrates(valvecount))
        allocate(this%tunnelcounts(valvecount))
        allocate(this%tunnels(maxtunnels,valvecount), source=0)

        ! extract names of valves
        do i = 1, valvecount
            line = lines(i)
            this%valves(i) = line(7:8)
        end do

        this%workingvalves = 0
        do i = 1, valvecount
            line = lines(i)

            ! adjust line if flow rate has only one digit
            if (line(25:25) == ';') then
                line(25:) = line(24:linelength-1)
                line(24:24) = ' '
            end if
            ! print *, line

            ! adjust line if only one tunnel exists
            if (line(49:49) == ' ') then
                line(50:) = line(49:linelength-1)
            end if
            ! print *, line

            ! extract flow rates
            read (line(24:25), *) this%flowrates(i)
            if (this%flowrates(i) > 0) then
                this%workingvalves = this%workingvalves + 1
            end if

            ! extract tunnels
            tunnels = trim(line(51:))
            tunnelcount = (len(tunnels) + 2) / 4
            this%tunnelcounts(i) = tunnelcount
            do j = 1, tunnelcount
                tunnel = tunnels(j*4-3:j*4-2)
                this%tunnels(j, i) = findloc(this%valves, tunnel, 1)
            end do
        end do

        ! print *, this%valves
        ! print *, this%flowrates
        ! print *, this%workingvalves
        ! do i = 1, valvecount
        !     print *, this%tunnels(:this%tunnelcounts(i),i)
        ! end do
    end subroutine

    subroutine print_path(thecave, valvespath, additionalpressure, valvesopenstring)
        implicit none

        type(Cave), intent(in) :: thecave
        integer, intent(in)    :: valvespath(:)
        integer, intent(in)    :: additionalpressure(:)
        character(len=100), intent(in) :: valvesopenstring(:)
        integer                :: minute

        do minute = 1, maxminutes
            write (*, '(I2, A1, A3, I4, A1, A)') (maxminutes - minute + 1), ' ', thecave%valves(valvespath(minute)), &
                                                 additionalpressure(minute), ' ', valvesopenstring(minute)
        end do
        print *
    end subroutine

    function create_valvesopenstring(thecave, valvesopen) result(valvesopenstring)
        implicit none

        type(Cave), intent(in) :: thecave
        logical, intent(in)    :: valvesopen(:)
        character(len=100)     :: valvesopenstring
        integer                :: i, j

        valvesopenstring = ''
        j = 1
        do i = 1, size(valvesopen)
            if (valvesopen(i) .eqv. .true.) then
                valvesopenstring(j:j+1) = thecave%valves(i)
                valvesopenstring(j+2:j+2) = ','
                j = j + 4
            end if
        end do

    end function

    recursive subroutine traverse(thecave, valvesopen, valvesopenstring, valvespath, additionalpressure, &
                                  minute, action, currentvalve, currentpressure, mostpressure)
        implicit none

        type(Cave), intent(in)            :: thecave
        logical, intent(inout)            :: valvesopen(:)
        character(len=100), intent(inout) :: valvesopenstring(:)
        integer, intent(inout)            :: valvespath(:)
        integer, intent(inout)            :: additionalpressure(:)
        integer, intent(in)               :: minute, currentvalve, currentpressure
        integer, intent(inout)            :: mostpressure
        character(len=*), intent(in)      :: action
        integer                           :: increasedpressure, i, nextvalve, skipminutes

        ! remember path
        valvespath(minute) = currentvalve

        ! increase pressure
        additionalpressure(minute) = sum(thecave%flowrates, 1, valvesopen)
        increasedpressure = currentpressure + additionalpressure(minute)
        ! print *, valvesopen, minute, action, currentvalve, thecave%valves(currentvalve), currentpressure, increasedpressure

        ! already at the last minute?
        if (minute == maxminutes) then
            if (mostpressure < increasedpressure) then
                ! print '(A, I4, A, I4)', 'last minute, new top score increased from ', mostpressure, ' to ', increasedpressure
                ! call print_path(thecave, valvespath, additionalpressure, valvesopenstring)
                mostpressure = increasedpressure
                ! stop
            end if
            return
        end if

        if (count(valvesopen) == thecave%workingvalves) then
            ! all valves are already open, no need to move anymore
            ! valvesopenstring(minute + 1) = valvesopenstring(minute)
            call traverse(thecave, valvesopen, valvesopenstring, valvespath, additionalpressure, &
                          minute + 1, 'skip', currentvalve, increasedpressure, mostpressure)
            ! nothing to be done anymore, so return
            return
        end if

        ! check if the valve could be opened
        if (thecave%flowrates(currentvalve) > 0) then
            if (.not. valvesopen(currentvalve)) then
                valvesopen(currentvalve) = .true.
                ! valvesopenstring(minute + 1) = create_valvesopenstring(thecave, valvesopen)
                call traverse(thecave, valvesopen, valvesopenstring, valvespath, additionalpressure, &
                              minute + 1, 'open', currentvalve, increasedpressure, mostpressure)
                valvesopen(currentvalve) = .false.
            end if
        end if

        ! try all the tunnels
        do i = 1, thecave%tunnelcounts(currentvalve)
            nextvalve = thecave%tunnels(i,currentvalve)
            if (minute >= 2) then
                if (nextvalve == valvespath(minute - 1)) then
                    ! do not go back directly
                    cycle
                end if
            end if
            if (thecave%tunnelcounts(nextvalve) == 1) then
                if (valvesopen(nextvalve)) then
                    ! do not go to an open valve with only one tunnel
                    cycle
                end if
            end if
            ! valvesopenstring(minute + 1) = valvesopenstring(minute)
            call traverse(thecave, valvesopen, valvesopenstring, valvespath, additionalpressure, &
                          minute + 1, 'move', nextvalve, increasedpressure, mostpressure)
        end do
    end subroutine

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)    :: filename
        character(len=:), allocatable   :: lines(:)
        type(Cave)                      :: thecave
        logical, allocatable            :: valvesopen(:)
        character(len=100), allocatable :: valvesopenstring(:)
        integer, allocatable            :: valvespath(:)
        integer, allocatable            :: additionalpressure(:)
        integer                         :: mostpressure, valvecount

        lines = readinputfile_asstringarray(filename, maxlinelength)
        valvecount = size(lines)
        allocate(valvesopen(valvecount), source=.false.)
        allocate(valvesopenstring(maxminutes))
        valvesopenstring = ''
        allocate(valvespath(maxminutes), source=0)
        allocate(additionalpressure(maxminutes), source=0)

        call thecave%init(lines)
        mostpressure = 0
        call traverse(thecave, valvesopen, valvesopenstring, valvespath, additionalpressure, &
                      1, 'strt', findloc(thecave%valves, 'AA', 1), 0, mostpressure)
        ! mostpressure = -1

        solve = mostpressure
    end function

end module day16a
