!> Solution for https://adventofcode.com/2021/day/15 part a
module day15a
    use iso_fortran_env, only : error_unit
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 80
    character(len=1), parameter :: char_empty = '.', char_sensor = 'S', char_beacon = 'B', char_proximity = '#'

    type :: SBMap
        private

        character(len=:), allocatable :: map(:)
        integer                       :: minx, maxx, miny, maxy
    contains
        procedure :: init      => sbmap_init
        procedure :: put       => sbmap_put
        procedure :: get       => sbmap_get
        procedure :: isfree    => sbmap_isfree
        procedure :: isvalid   => sbmap_isvalid
        procedure :: drawwalls => sbmap_drawwalls
        procedure :: print     => sbmap_print
    end type SBMap

    public :: solve

contains

    subroutine sbmap_init(this, coords)
        implicit none

        class(SBMap), intent(inout) :: this
        integer, intent(in)           :: coords(:)
        integer                       :: linelength
        character(len=:), allocatable :: fillline
        integer                       :: i

        ! calculate dimensions of map
        this%minx = minval(coords(::2))
        this%maxx = maxval(coords(::2))
        this%miny = minval(coords(2::2))
        this%maxy = maxval(coords(2::2))
        print *, this%minx, this%maxx, this%miny, this%maxy

        ! allocate map and fill with dots
        linelength = this%maxx-this%minx+1
        allocate(character(len=linelength) :: this%map(this%miny:this%maxy))
        fillline = repeat(char_empty, linelength)
        this%map(:) = fillline

        ! mark sensors and beacons
        do i = 1, size(coords), 4
            call this%put(char_sensor, coords(i), coords(i+1))
            call this%put(char_beacon, coords(i+2), coords(i+3))
        end do
    end subroutine

    subroutine sbmap_put(this, char, x, y, allowoverwrite)
        implicit none

        class(SBMap), intent(inout) :: this
        character(len=1), intent(in)  :: char
        integer, intent(in)           :: x, y
        logical, intent(in), optional :: allowoverwrite
        integer                       :: xfixed
        logical                       :: maketest
        character(len=1)              :: testchar

        xfixed = x - this%minx + 1
        maketest = .true.
        if (present(allowoverwrite)) then
            if (allowoverwrite .eqv. .true.) then
                maketest = .false.
            end if
        end if
        if (maketest) then
            testchar = this%map(y)(xfixed:xfixed)
            if (testchar /= char .and. testchar /= char_empty) then
                write (error_unit, *) 'SBMap error for put(): coords are not empty (', x, '/', y, ')'
                stop
            end if
        end if
        this%map(y)(xfixed:xfixed) = char
    end subroutine

    function sbmap_get(this, x, y) result(char)
        implicit none

        class(SBMap), intent(inout) :: this
        integer, intent(in)           :: x, y
        character(len=1)              :: char
        integer                       :: xfixed

        xfixed = x - this%minx + 1
        char = this%map(y)(xfixed:xfixed)
    end function

    function sbmap_isfree(this, x, y) result(isfree)
        implicit none

        class(SBMap), intent(inout) :: this
        integer, intent(in)           :: x, y
        logical                       :: isfree

        isfree = this%get(x, y) == char_empty
    end function

    function sbmap_isvalid(this, x, y) result(isvalid)
        implicit none

        class(SBMap), intent(inout) :: this
        integer, intent(in)           :: x, y
        logical                       :: isvalid

        isvalid = x >= this%minx .and. x <= this%maxx .and. y >= this%miny .and. y <= this%maxy
    end function

    subroutine sbmap_drawwalls(this, coords)
        implicit none

        class(SBMap), intent(inout) :: this
        integer, intent(in)           :: coords(:)
        integer                       :: i, x, y, lastx, lasty, drawx, drawy

        lastx = -1
        lasty = -1
        do i = 1, size(coords), 2
            x = coords(i)
            y = coords(i+1)
            if (lastx == x .and. lasty == y) then
                ! end of current structure
                lastx = -1
                lasty = -1
            else
                if (lastx /= -1) then
                    ! not the first coords, so draw a wall
                    do drawy = lasty, y, sign(1, y-lasty)
                        do drawx = lastx, x, sign(1, x-lastx)
!                            call this%put(char_wall, drawx, drawy, .true.)
                        end do
                    end do
                end if
                lastx = x
                lasty = y
            end if
        end do
    end subroutine

    subroutine sbmap_print(this)
        implicit none

        class(SBMap), intent(inout) :: this
        integer                     :: y

        do y = lbound(this%map, 1), ubound(this%map, 1)
            print '(A)', this%map(y)
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

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer, allocatable          :: coords(:)
        type(SBMap)                   :: map
        integer                       :: impossible_positions

        lines = readinputfile_asstringarray(filename, maxlinelength)

        call extract_coords(lines, coords)
        print *, coords

        call map%init(coords)
        call map%print()
        ! impossible_positions = find_impossible_positions(map)
        impossible_positions = -1

        solve = impossible_positions
    end function

end module day15a
