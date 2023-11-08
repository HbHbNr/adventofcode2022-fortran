!> Solution for https://adventofcode.com/2021/day/14 part a
module day14a
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 150
    integer, parameter :: avgcoordsperline = 9

    public :: solve

contains

    subroutine extract_coords(lines, coords)
        character(len=*), intent(in)      :: lines(:)
        integer, intent(out), allocatable :: coords(:)
        character(len=maxlinelength)      :: line
        character(len=1)                  :: char
        integer                           :: i, j, c, count

        ! average of "avgcoordsperline" coords per line
        allocate(coords(size(lines) * avgcoordsperline * 2), source=-1)

        ! scan lines an save coords
        c = 0
        do i = 1, size(lines)
            line = lines(i)
            print *, line
            count = 0
            do j = 1, len_trim(line)
                char = line(j:j)
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

            ! keep one value -1 between coords
            c = c + count + 1
        end do
        print *, coords

    end subroutine

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer, allocatable          :: coords(:)

        lines = readinputfile_asstringarray(filename, maxlinelength)

        call extract_coords(lines, coords)

        solve = -1
    end function

end module day14a
