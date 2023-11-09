!> Solution for https://adventofcode.com/2021/day/14 part a
module day14a
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 150
    integer, parameter :: avgcoordsperline = 10

    type :: SandMap
        private

        character(len=:), allocatable :: map(:)
        integer                       :: minx, maxx, miny, maxy
    contains
        procedure :: init      => sandmap_init
        procedure :: put       => sandmap_put
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
        fillline = repeat('.', linelength)
        this%map(:) = fillline

        ! mark source of sand and draw rock structures
        call this%put('+', 500, 0)
        call this%drawwalls(coords)

        call this%print()
    end subroutine

    subroutine sandmap_put(this, char, x, y)
        implicit none

        class(SandMap), intent(inout) :: this
        character(len=1), intent(in)  :: char
        integer, intent(in)           :: x, y
        integer                       :: xfixed

        xfixed = x - this%minx + 1
        this%map(y)(xfixed:xfixed) = char
    end subroutine

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
                            call this%put('#', drawx, drawy)
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
            ! print *, line
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
            ! print *, 'count=', count
            ! print *, line

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

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer, allocatable          :: coords(:)
        type(SandMap)                 :: map

        lines = readinputfile_asstringarray(filename, maxlinelength)

        call extract_coords(lines, coords)
        ! print *, coords

        call map%init(coords)

        solve = -1
    end function

end module day14a
