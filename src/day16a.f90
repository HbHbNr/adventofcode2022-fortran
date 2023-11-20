!> Solution for https://adventofcode.com/2021/day/16 part a
module day16a
    USE ieee_arithmetic
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

    type :: GraphFW
        private

        real, allocatable :: distance(:,:)
    contains
        procedure :: init       => graphfw_init
        procedure :: add        => graphfw_add
        procedure :: find_paths => graphfw_find_paths
    end type GraphFW

    public :: solve

contains

    subroutine graphfw_init(this, number_of_nodes)
        implicit none

        class(GraphFW), intent(inout) :: this
        integer, intent(in)           :: number_of_nodes
        integer                       :: i

        allocate(this%distance(number_of_nodes, number_of_nodes))
        this%distance(:,:) = ieee_value(0.0, ieee_positive_inf)  ! initial all paths with positive infinity, e.g. no connection
        do i = 1, number_of_nodes
            this%distance(i,i) = 0
        end do
    end subroutine

    subroutine graphfw_add(this, from, to, distance)
        implicit none

        class(GraphFW), intent(inout) :: this
        integer, intent(in)           :: from, to
        real, intent(in)              :: distance

        this%distance(from,to) = distance
    end subroutine

    subroutine graphfw_find_paths(this)
        implicit none

        class(GraphFW), intent(inout) :: this
        integer                       :: i, j, k
        integer                       :: number_of_nodes

        number_of_nodes = size(this%distance, 1)
        do k = 1, number_of_nodes
            do i = 1, number_of_nodes
                do j = 1, number_of_nodes
                    this%distance(i, j) = min(this%distance(i, j), this%distance(i, k) + this%distance(k, j))
                end do
            end do
        end do
    end subroutine

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

    ! subroutine print_path(thecave, valvespath)
    !     implicit none

    !     type(Cave), intent(in) :: thecave
    !     integer, intent(in)    :: valvespath(:)
    !     integer                :: minute

    !     do minute = 1, maxminutes
    !         write (*, '(I2, A1, A3, I4)') (maxminutes - minute + 1), ' ', thecave%valves(valvespath(minute))
    !     end do
    !     print *
    ! end subroutine

    ! function create_valvesopenstring(thecave, valvesopen) result(valvesopenstring)
    !     implicit none

    !     type(Cave), intent(in) :: thecave
    !     logical, intent(in)    :: valvesopen(:)
    !     character(len=100)     :: valvesopenstring
    !     integer                :: i, j

    !     valvesopenstring = ''
    !     j = 1
    !     do i = 1, size(valvesopen)
    !         if (valvesopen(i) .eqv. .true.) then
    !             valvesopenstring(j:j+1) = thecave%valves(i)
    !             valvesopenstring(j+2:j+2) = ','
    !             j = j + 4
    !         end if
    !     end do

    ! end function

    ! recursive subroutine traverse(thecave, valvesopen, valvespath, &
    !                               minute, currentvalve, currentpressure, mostpressure)
    !     implicit none

    !     type(Cave), intent(in)            :: thecave
    !     logical, intent(inout)            :: valvesopen(:)
    !     integer, intent(inout)            :: valvespath(:)
    !     integer, intent(in)               :: minute, currentvalve, currentpressure
    !     integer, intent(inout)            :: mostpressure
    !     integer                           :: additionalpressure, increasedpressure, i, nextvalve

    !     ! remember path
    !     valvespath(minute) = currentvalve

    !     ! increase pressure
    !     additionalpressure = sum(thecave%flowrates, 1, valvesopen)
    !     increasedpressure = currentpressure + additionalpressure
    !     ! print *, valvesopen, minute, currentvalve, thecave%valves(currentvalve), currentpressure, increasedpressure

    !     ! already at the last minute?
    !     if (minute == maxminutes) then
    !         if (mostpressure < increasedpressure) then
    !             ! print '(A, I4, A, I4)', 'last minute, new top score increased from ', mostpressure, ' to ', increasedpressure
    !             ! call print_path(thecave, valvespath)
    !             mostpressure = increasedpressure
    !             ! stop
    !         end if
    !         return
    !     end if

    !     if (count(valvesopen) == thecave%workingvalves) then
    !         ! all valves are already open, no need to move anymore
    !         call traverse(thecave, valvesopen, valvespath, &
    !                       minute + 1, currentvalve, increasedpressure, mostpressure)
    !         ! nothing to be done anymore, so return
    !         return
    !     end if

    !     ! check if the valve could be opened
    !     if (thecave%flowrates(currentvalve) > 0) then
    !         if (.not. valvesopen(currentvalve)) then
    !             valvesopen(currentvalve) = .true.
    !             call traverse(thecave, valvesopen, valvespath, &
    !                           minute + 1, currentvalve, increasedpressure, mostpressure)
    !             valvesopen(currentvalve) = .false.
    !         end if
    !     end if

    !     ! try all the tunnels
    !     do i = 1, thecave%tunnelcounts(currentvalve)
    !         nextvalve = thecave%tunnels(i,currentvalve)
    !         if (minute >= 2) then
    !             if (nextvalve == valvespath(minute - 1)) then
    !                 ! do not go back directly
    !                 cycle
    !             end if
    !         end if
    !         if (thecave%tunnelcounts(nextvalve) == 1) then
    !             if (valvesopen(nextvalve)) then
    !                 ! do not go to an open valve with only one tunnel
    !                 cycle
    !             end if
    !         end if
    !         call traverse(thecave, valvesopen, valvespath, &
    !                       minute + 1, nextvalve, increasedpressure, mostpressure)
    !     end do
    ! end subroutine

    subroutine init_graph(thecave, graph)
        implicit none

        type(Cave), intent(in)       :: thecave
        type(GraphFW), intent(inout) :: graph
        integer                      :: i, j, tunnel

        ! set distances to default
        call graph%init(size(thecave%valves))

        ! add direct paths
        do i = 1, size(thecave%valves, 1)
            do j = 1, thecave%tunnelcounts(i)
                tunnel = thecave%tunnels(j,i)
                ! this%tunnels(j, i) = findloc(this%valves, tunnel, 1)
                call graph%add(i, tunnel, 1.0)
            end do
        end do

        ! find path in graph using Floyd–Warshall algorithm
        call graph%find_paths()
    end subroutine

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)    :: filename
        character(len=:), allocatable   :: lines(:)
        type(Cave)                      :: thecave
        type(GraphFW)                   :: graph
        ! logical, allocatable            :: valvesopen(:)
        ! integer, allocatable            :: valvespath(:)
        integer                         :: mostpressure
        ! integer                         :: valvecount

        lines = readinputfile_asstringarray(filename, maxlinelength)
        ! valvecount = size(lines)
        ! allocate(valvesopen(valvecount), source=.false.)
        ! allocate(valvespath(maxminutes), source=0)

        call thecave%init(lines)
        call init_graph(thecave, graph)
        print *, graph%distance
        mostpressure = 0
        ! call traverse(thecave, valvesopen, valvespath, &
        !               1, findloc(thecave%valves, 'AA', 1), 0, mostpressure)

        mostpressure = -1
        solve = mostpressure
    end function

end module day16a
