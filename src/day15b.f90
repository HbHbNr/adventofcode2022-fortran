!> Solution for https://adventofcode.com/2021/day/15 part b
module day15b
    use iso_fortran_env, only : error_unit, int64
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 80

    public :: solve

contains

    subroutine extract_coords(lines, coords)
        character(len=*), intent(in)      :: lines(:)
        integer, intent(out), allocatable :: coords(:)
        character(len=maxlinelength)      :: line
        character(len=1)                  :: char
        integer                           :: i, j, c, ichar0 = ichar('0'), ichar9 = ichar('9')

        ! 5 coords per line
        allocate(coords(size(lines) * 5))

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

            ! read coords of sensor and beacon into the correct places of the array
            read (line, *) coords(c + 1:c + 4)

            ! add distance between sensor and beacon as 5th coordinate
            coords(c + 5) = abs(coords(c + 3)-coords(c + 1)) + abs(coords(c + 4)-coords(c + 2))

            c = c + 5
        end do
    end subroutine

    function find_tuning_frequency(coords, maxcoord) result(tuning_frequency)
        implicit none

        integer, intent(in) :: coords(:)
        integer, intent(in) :: maxcoord
        integer             :: totalcoords
        integer(int64)      :: tuning_frequency
        integer             :: sensor1, sensor1x, sensor1y, distance1
        integer             :: sensor2, sensor2x, sensor2y, distance2
        integer, parameter  :: minx = 0, miny = 0
        integer             :: maxx, maxy
        integer             :: xs(2), xsi, x, y, inspect1x, inspect1y, inspectdistance

        maxx = maxcoord
        maxy = maxcoord
        totalcoords = size(coords)
        sensor1loop: do sensor1 = 1, totalcoords - 5, 5
            sensor1x = coords(sensor1)
            sensor1y = coords(sensor1 + 1)
            distance1 = coords(sensor1 + 4)
            ! one y step further than distance, because the missing beacon is undetected
            yloop: do y = -distance1 - 1, distance1 + 1
                inspect1y = sensor1y + y
                if (inspect1y < miny) then
                    ! whole line is out of valid y range
                    cycle yloop
                else if (inspect1y > maxy) then
                    ! whole line is out of valid y range
                    cycle yloop
                end if
                ! also one x step further
                xs = [-(distance1-abs(y))-1, distance1-abs(y)+1]
                xloop: do xsi = 1, 2
                    x = xs(xsi)
                    inspect1x = sensor1x + x
                    if (inspect1x < minx) then
                        ! this position is out of valid x range
                        cycle xloop
                    else if (inspect1x > maxx) then
                        ! this position is out of valid x range
                        cycle xloop
                    end if
                    sensor2loop: do sensor2 = 1, totalcoords, 5
                        if (sensor1 == sensor2) then
                            ! don't compare sensor with itself
                            cycle sensor2loop
                        end if
                        sensor2x = coords(sensor2)
                        sensor2y = coords(sensor2 + 1)
                        distance2 = coords(sensor2 + 4)
                        inspectdistance = abs(inspect1x-sensor2x) + abs(inspect1y-sensor2y)
                        if (inspectdistance <= distance2) then
                            ! inspected position is in reach of sensor 2, so continue with next position to check
                            cycle xloop
                        end if
                    end do sensor2loop
                    ! inspected position is not reachable, so this is the result!
                    tuning_frequency = int(inspect1x, int64) * 4000000 + inspect1y
                    return
                end do xloop
            end do yloop
        end do sensor1loop
    end function

    integer(int64) function solve(filename, maxcoord)
        implicit none

        character(len=*), intent(in)  :: filename
        integer, intent(in)           :: maxcoord
        character(len=:), allocatable :: lines(:)
        integer, allocatable          :: coords(:)
        integer(int64)                :: tuning_frequency

        lines = readinputfile_asstringarray(filename, maxlinelength)

        call extract_coords(lines, coords)

        ! do not create a map, but check all possible positions
        tuning_frequency = find_tuning_frequency(coords, maxcoord)

        solve = tuning_frequency
    end function

end module day15b
