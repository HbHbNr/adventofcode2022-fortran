!> Solution for https://adventofcode.com/2021/day/16 part a
module day16a
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 68
    integer, parameter :: maxtunnels = 5

    type :: Cave
        private

        character(len=2), allocatable :: valves(:)
        integer, allocatable          :: flowrates(:)
        integer, allocatable          :: tunnelcounts(:)
        integer, allocatable          :: tunnels(:,:)
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

        !
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
        ! do i = 1, valvecount
        !     print *, this%tunnels(:this%tunnelcounts(i),i)
        ! end do
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
