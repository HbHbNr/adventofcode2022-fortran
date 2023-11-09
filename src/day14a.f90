!> Solution for https://adventofcode.com/2021/day/14 part a
module day14a
    use iso_fortran_env, only : error_unit
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 150
    integer, parameter :: avgcoordsperline = 10
    integer, parameter :: source_x = 500, source_y = 0
    character(len=1), parameter :: char_empty = '.', char_sand = 'o', char_source = '+', char_wall = '#'

    type :: SandMap
        private

        character(len=:), allocatable :: map(:)
        integer                       :: minx, maxx, miny, maxy
    contains
        procedure :: init      => sandmap_init
        procedure :: put       => sandmap_put
        procedure :: get       => sandmap_get
        procedure :: isfree    => sandmap_isfree
        procedure :: isvalid   => sandmap_isvalid
        procedure :: drawwalls => sandmap_drawwalls
        procedure :: print     => sandmap_print
    end type SandMap

    public :: solve

contains

    subroutine sandmap_init(this, coords)
        implicit none

        class(SandMap), intent(inout) :: this
        integer, intent(in)           :: coords(:)
        integer                       :: linelength
        character(len=:), allocatable :: fillline

        ! calculate dimensions of map
        this%minx = minval(coords(::2))
        this%maxx = maxval(coords(::2))
        this%miny = 0  ! source of sand
        this%maxy = maxval(coords(2::2))

        ! allocate map and fill with dots
        linelength = this%maxx-this%minx+1
        allocate(character(len=linelength) :: this%map(this%miny:this%maxy))
        fillline = repeat(char_empty, linelength)
        this%map(:) = fillline

        ! mark source of sand and draw rock structures
        call this%put(char_source, source_x, source_y)
        call this%drawwalls(coords)
    end subroutine

    subroutine sandmap_put(this, char, x, y, allowoverwrite)
        implicit none

        class(SandMap), intent(inout) :: this
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
            if (testchar /= char_empty) then
                write (error_unit, *) 'SandMap error for put(): coords are not empty (', x, '/', y, ')'
                stop
            end if
        end if
        this%map(y)(xfixed:xfixed) = char
    end subroutine

    function sandmap_get(this, x, y) result(char)
        implicit none

        class(SandMap), intent(inout) :: this
        integer, intent(in)           :: x, y
        character(len=1)              :: char
        integer                       :: xfixed

        xfixed = x - this%minx + 1
        char = this%map(y)(xfixed:xfixed)
    end function

    function sandmap_isfree(this, x, y) result(isfree)
        implicit none

        class(SandMap), intent(inout) :: this
        integer, intent(in)           :: x, y
        logical                       :: isfree

        isfree = this%get(x, y) == char_empty
    end function

    function sandmap_isvalid(this, x, y) result(isvalid)
        implicit none

        class(SandMap), intent(inout) :: this
        integer, intent(in)           :: x, y
        logical                       :: isvalid

        isvalid = x >= this%minx .and. x <= this%maxx .and. y >= this%miny .and. y <= this%maxy
    end function

    subroutine sandmap_drawwalls(this, coords)
        implicit none

        class(SandMap), intent(inout) :: this
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
                            call this%put(char_wall, drawx, drawy, .true.)
                        end do
                    end do
                end if
                lastx = x
                lasty = y
            end if
        end do
    end subroutine

    subroutine sandmap_print(this)
        implicit none

        class(SandMap), intent(inout) :: this
        integer                       :: y

        do y = lbound(this%map, 1), ubound(this%map, 1)
            print '(A)', this%map(y)
        end do
    end subroutine

    subroutine extract_coords(lines, coords)
        character(len=*), intent(in)      :: lines(:)
        integer, intent(out), allocatable :: coords(:)
        character(len=maxlinelength)      :: line
        character(len=1)                  :: char
        integer                           :: i, j, c, count

        ! average of "avgcoordsperline" coords per line
        allocate(coords(size(lines) * avgcoordsperline * 2), source=0)

        ! scan lines an save coords
        c = 0  ! number of single coords saved into linear array
        do i = 1, size(lines)
            line = lines(i)
            count = 0
            do j = 1, len_trim(line)
                char = line(j:j)
                ! count coord pairs and override non-digits
                select case (char)
                case (',')
                    count = count + 2
                    line(j:j) = ' '
                case ('-', '>')
                    line(j:j) = ' '
                end select
            end do

            ! read coords into the correct places of the array
            read (line, *) coords(c + 1:c + count)
            c = c + count

            ! duplicate the two values of the last coordinate to signal the end of the line segments
            coords(c+1:c+2) = coords(c-1:c)
            c = c + 2
        end do

        ! shrink coords array to the actual number of values
        coords = coords(1:c)
    end subroutine

    function simulate_sand(map) result(sandunits)
        implicit none

        type(SandMap), intent(inout) :: map
        integer                      :: sandunits
        logical                      :: falling
        integer                      :: x, y, nextxs(3), nextx, nexty, i

        sandunits = 0
        producing_loop: do
            sandunits = sandunits + 1
            x = source_x
            y = source_y
            falling = .true.
            do while (falling)
                nextxs = [x, x-1, x+1]
                nexty = y + 1
                falling = .false.
                possible_next: do i = 1, 3
                    ! try three possible next positions
                    nextx = nextxs(i)
                    if (.not. map%isvalid(nextx, nexty)) then
                        ! first sand unit has reached an invalid field
                        sandunits = sandunits - 1
                        exit producing_loop
                    end if
                    if (map%isfree(nextx, nexty)) then
                        ! position is free, so move sand unit there, and keep falling
                        x = nextx
                        y = nexty
                        falling = .true.
                        exit possible_next
                    end if
                end do possible_next
                ! if none of the three possible next positions is free, then "falling is now false"
            end do
            call map%put(char_sand, x, y)
        end do producing_loop
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer, allocatable          :: coords(:)
        type(SandMap)                 :: map
        integer                       :: sandunits

        lines = readinputfile_asstringarray(filename, maxlinelength)

        call extract_coords(lines, coords)

        call map%init(coords)
        sandunits = simulate_sand(map)

        solve = sandunits
    end function

end module day14a
