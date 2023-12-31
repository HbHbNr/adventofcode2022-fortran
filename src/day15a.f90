!> Solution for https://adventofcode.com/2022/day/15 part a
module day15a
    use iso_fortran_env, only : error_unit
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 80
    character(len=1), parameter :: char_empty = '.', char_sensor = 'S', char_beacon = 'B', char_coverage = '#'

    type :: SBMap
        private

        character(len=:), allocatable :: map(:)
        integer                       :: minx, maxx, miny, maxy, yline
    contains
        procedure :: init         => sbmap_init
        procedure :: put          => sbmap_put
        procedure :: get          => sbmap_get
        procedure :: isfree       => sbmap_isfree
        procedure :: isvalid      => sbmap_isvalid
        procedure :: drawcoverage => sbmap_drawcoverage
        procedure :: print        => sbmap_print
    end type SBMap

    public :: solve

contains

    subroutine sbmap_init(this, coords, yline)
        implicit none

        class(SBMap), intent(inout)   :: this
        integer, intent(in)           :: coords(:)
        integer, intent(in)           :: yline
        integer                       :: linelength
        character(len=:), allocatable :: fillline

        ! calculate dimensions of map
        this%minx = minval(coords(::2)) - 10000000
        this%maxx = maxval(coords(::2)) + 10000000
        this%miny = minval(coords(2::2)) - 10000000
        this%maxy = maxval(coords(2::2)) + 10000000
        this%yline = yline

        ! allocate map and fill with dots
        linelength = this%maxx-this%minx+1
        allocate(character(len=linelength) :: this%map(this%yline:this%yline))
        fillline = repeat(char_empty, linelength)
        this%map(:) = fillline

        ! mark sensors and beacons
        call this%drawcoverage(coords)
    end subroutine

    subroutine sbmap_put(this, char, x, y)
        implicit none

        class(SBMap), intent(inout)  :: this
        character(len=1), intent(in) :: char
        integer, intent(in)          :: x, y
        integer                      :: xfixed

        if (y /= this%yline) then
            ! put only characters onto the interesting line
            return
        end if
        xfixed = x - this%minx + 1
        this%map(y)(xfixed:xfixed) = char
    end subroutine

    function sbmap_get(this, x, y) result(char)
        implicit none

        class(SBMap), intent(inout) :: this
        integer, intent(in)         :: x, y
        character(len=1)            :: char
        integer                     :: xfixed

        xfixed = x - this%minx + 1
        char = this%map(y)(xfixed:xfixed)
    end function

    function sbmap_isfree(this, x, y) result(isfree)
        implicit none

        class(SBMap), intent(inout) :: this
        integer, intent(in)         :: x, y
        logical                     :: isfree

        if (y /= this%yline) then
            ! every non-interesting line is free
            isfree = .true.
        else
            isfree = this%get(x, y) == char_empty
        end if
    end function

    function sbmap_isvalid(this, x, y) result(isvalid)
        implicit none

        class(SBMap), intent(inout) :: this
        integer, intent(in)           :: x, y
        logical                       :: isvalid

        isvalid = x >= this%minx .and. x <= this%maxx .and. y >= this%miny .and. y <= this%maxy
    end function

    subroutine sbmap_drawcoverage(this, coords)
        implicit none

        class(SBMap), intent(inout) :: this
        integer, intent(in)         :: coords(:)
        integer                     :: i, sensorx, sensory, beaconx, beacony, distance, x, y

        do i = 1, size(coords), 4
            sensorx = coords(i)
            sensory = coords(i+1)
            beaconx = coords(i+2)
            beacony = coords(i+3)
            distance = abs(beaconx-sensorx) + abs(beacony-sensory)
            if (abs(sensory - this%yline) > distance) then
                ! sensor coverage is too far away from yline, so ignore it completely
                cycle
            end if
            do y = -distance, distance
                if (sensory + y /= this%yline) then
                    cycle
                end if
                do x = -(distance-abs(y)), distance-abs(y)
                    call this%put(char_coverage, sensorx + x, sensory + y)
                end do
            end do
        end do
        ! restore sensors and beacons
        do i = 1, size(coords), 4
            sensorx = coords(i)
            sensory = coords(i+1)
            beaconx = coords(i+2)
            beacony = coords(i+3)
            call this%put(char_sensor, sensorx, sensory)
            call this%put(char_beacon, beaconx, beacony)
        end do
    end subroutine

    subroutine sbmap_print(this)
        implicit none

        class(SBMap), intent(inout) :: this
        integer                     :: y

        do y = lbound(this%map, 1), ubound(this%map, 1)
            print '(I2, A, A)', y, ' ', this%map(y)
        end do
    end subroutine

    subroutine extract_coords(lines, coords)
        character(len=*), intent(in)      :: lines(:)
        integer, intent(out), allocatable :: coords(:)
        character(len=maxlinelength)      :: line
        character(len=1)                  :: char
        integer                           :: i, j, c, ichar0 = ichar('0'), ichar9 = ichar('9')

        ! 4 coords per line
        allocate(coords(size(lines) * 4))

        ! scan lines an save coords
        c = 0  ! number of single coords saved into linear array
        do i = 1, size(lines)
            line = lines(i)
            do j = 1, len_trim(line)
                char = line(j:j)
                if (char /= '-') then
                    if (ichar(char) < ichar0 .or. ichar(char) > ichar9) then
                        line(j:j) = ' '
                    end if
                end if
            end do

            ! read coords into the correct places of the array
            read (line, *) coords(c + 1:c + 4)
            c = c + 4
        end do
    end subroutine

    function find_impossible_positions(map) result(impossible_positions)
        implicit none

        type(SBMap), intent(inout) :: map
        integer                    :: impossible_positions
        integer                    :: x

        impossible_positions = 0
        do x = map%minx, map%maxx
            if (map%get(x, map%yline) == char_coverage) then
                impossible_positions = impossible_positions + 1
            end if
        end do
    end function

    integer function solve(filename, yline)
        implicit none

        character(len=*), intent(in)  :: filename
        integer, intent(in)           :: yline
        character(len=:), allocatable :: lines(:)
        integer, allocatable          :: coords(:)
        type(SBMap)                   :: map
        integer                       :: impossible_positions

        lines = readinputfile_asstringarray(filename, maxlinelength)

        call extract_coords(lines, coords)

        call map%init(coords, yline)
        impossible_positions = find_impossible_positions(map)

        solve = impossible_positions
    end function

end module day15a
