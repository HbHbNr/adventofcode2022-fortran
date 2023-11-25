!> Solution for https://adventofcode.com/2021/day/16 part b
module day16b
    USE ieee_arithmetic
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 68
    integer, parameter :: maxtunnels = 5
    integer, parameter :: maxminutes = 26

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
        ! graph for Floyd-Warshall algorithm
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

    recursive subroutine rectraverse(thecave, graph, mostpressure, pathpressure, minutelog1, minutelog2, &
                                     possibletargets, minutesleft1, minutesleft2, currentvalve1, currentvalve2)
        implicit none

        type(Cave), intent(in)          :: thecave
        type(GraphFW), intent(in)       :: graph
        integer, intent(inout)          :: mostpressure
        integer, intent(in)             :: pathpressure
        character(len=*), intent(inout) :: minutelog1(maxminutes), minutelog2(maxminutes)
        logical, intent(inout)          :: possibletargets(:)
        integer, intent(in)             :: minutesleft1, minutesleft2
        integer, intent(in)             :: currentvalve1, currentvalve2

        integer                         :: yield, distance
        integer                         :: minutesleft, actor, testvalve
        integer                         :: currentvalve

        ! is the pressure of the current path the highest measured up to now?
        if (mostpressure < pathpressure) then
            write (*, '(I3, I3, A, I4, A, I4)') minutesleft1, minutesleft2, &
                                            ' new top score increased from ', mostpressure, ' to ', pathpressure
            ! print *, minutelog1
            ! print *, minutelog2
            ! print *
            mostpressure = pathpressure
        end if

        ! work only with the person having more time left
        if (minutesleft1 >= minutesleft2) then
            minutesleft = minutesleft1
            currentvalve = currentvalve1
            actor = 1
        else
            minutesleft = minutesleft2
            currentvalve = currentvalve2
            actor = 2
        end if

        do testvalve = 1, size(possibletargets)
            ! testvalve has no valve to open
            if (.not. possibletargets(testvalve)) cycle 

            distance = int(graph%distance(currentvalve, testvalve))
            ! not reasonable for the remaining time
            if (distance + 1 >= minutesleft) cycle

            ! print ('(I2, A3, A3, I2)'), actor, thecave%valves(currentvalve), thecave%valves(testvalve), distance

            yield = (minutesleft - distance - 1) * thecave%flowrates(testvalve)
            possibletargets(testvalve) = .false.
            if (actor == 1) then
                write (minutelog1(minutesleft - distance), '(I2,A1,A2,A1)') &
                      minutesleft - distance, 'g', thecave%valves(testvalve), ' '
                write (minutelog1(minutesleft - distance - 1), '(I2,A1,A2,A1)') &
                      minutesleft - distance - 1, 'o', thecave%valves(testvalve), ' '
                call rectraverse(thecave, graph, mostpressure, pathpressure + yield, minutelog1, minutelog2, &
                                 possibletargets, minutesleft - distance - 1, minutesleft2, testvalve, currentvalve2)
                ! back tracking: revert last change
                possibletargets(testvalve) = .true.
                write (minutelog1(minutesleft - distance), *) ''
                write (minutelog1(minutesleft - distance - 1), *) ''
            else
                write (minutelog2(minutesleft - distance), '(I2,A1,A2,A1)') &
                      minutesleft - distance, 'g', thecave%valves(testvalve), ' '
                write (minutelog2(minutesleft - distance - 1), '(I2,A1,A2,A1)') &
                      minutesleft - distance - 1, 'o', thecave%valves(testvalve), ' '
                call rectraverse(thecave, graph, mostpressure, pathpressure + yield, minutelog1, minutelog2, &
                                 possibletargets, minutesleft1, minutesleft - distance - 1, currentvalve1, testvalve)
                ! back tracking: revert last change
                possibletargets(testvalve) = .true.
                write (minutelog2(minutesleft - distance), *) ''
                write (minutelog2(minutesleft - distance - 1), *) ''
            end if
        end do
    end subroutine

    subroutine traverse(thecave, graph, mostpressure)
        implicit none

        type(Cave), intent(in)    :: thecave
        type(GraphFW), intent(in) :: graph
        integer, intent(inout)    :: mostpressure
        character(len=6)          :: minutelog1(maxminutes), minutelog2(maxminutes)
        logical, allocatable      :: possibletargets(:)
        integer                   :: startvalve

        ! initialize possible targets
        startvalve = findloc(thecave%valves, 'AA', 1)
        minutelog1 = ''
        minutelog2 = ''
        write (minutelog1(maxminutes), '(I2,A1,A2,A1)') &
              maxminutes, 's', thecave%valves(startvalve), ' '
        write (minutelog2(maxminutes), '(I2,A1,A2,A1)') &
              maxminutes, 's', thecave%valves(startvalve), ' '
        allocate(possibletargets(size(thecave%valves)), source=.false.)
        possibletargets = thecave%flowrates > 0

        ! traverse recursively
        call rectraverse(thecave, graph, mostpressure, 0, &
                         minutelog1, minutelog2, possibletargets, maxminutes, maxminutes, startvalve, startvalve)
    end subroutine

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)    :: filename
        character(len=:), allocatable   :: lines(:)
        type(Cave)                      :: thecave
        type(GraphFW)                   :: graph
        integer                         :: mostpressure

        lines = readinputfile_asstringarray(filename, maxlinelength)

        call thecave%init(lines)
        call init_graph(thecave, graph)
        ! print *, graph%distance
        mostpressure = 0
        call traverse(thecave, graph, mostpressure)

        ! mostpressure = -1
        solve = mostpressure
    end function

end module day16b
