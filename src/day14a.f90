!> Solution for https://adventofcode.com/2021/day/14 part a
module day14a
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 150
    integer, parameter :: avgcoordsperline = 10

    public :: solve

contains

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
            print *, line
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
            print *, 'count=', count
            print *, line
            read (line, *) coords(c + 1:c + count)
            c = c + count

            ! duplicate the two values of the last coordinate to signal the end of the line segments
            coords(c+1:c+2) = coords(c-1:c)
            c = c + 2
        end do

        ! shrink coords array to the actual number of values
        coords = coords(1:c)
    end subroutine

    function simulate_sand(coords) result(sandlimit)
        implicit none

        integer, intent(in) :: coords(:)
        integer             :: sandlimit
        integer             :: minx, maxx, miny, maxy

        minx = minval(coords(::2))
        maxx = maxval(coords(::2))
        miny = minval(coords(2::2))
        maxy = maxval(coords(2::2))
        print *, minx, maxx, miny, maxy
        sandlimit = -1
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer, allocatable          :: coords(:)
        integer                       :: sandlimit

        lines = readinputfile_asstringarray(filename, maxlinelength)

        call extract_coords(lines, coords)
        print *, coords

        sandlimit = simulate_sand(coords)

        solve = -1
    end function

end module day14a
