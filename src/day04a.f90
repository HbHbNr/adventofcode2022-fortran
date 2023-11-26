!> Solution for https://adventofcode.com/2022/day/4 part a
module day04a
    use util, only : printioerror
    implicit none
    private

    public :: fully_contains
    public :: solve

contains

    integer function fully_contains(range1a, range1b, range2a, range2b)
        implicit none

        integer, intent(in) :: range1a, range1b, range2a, range2b

        if (((range1a <= range2a) .and. (range1b >= range2b)) &
            .or. &
            ((range1a >= range2a) .and. (range1b <= range2b))) then
            fully_contains = 1
        else
            fully_contains = 0
        end if
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in) :: filename
        character(len=11)            :: line
        integer                      :: io, iostat
        character(len=512)           :: iomsg
        integer                      :: i, dashpos, range1a, range1b, range2a, range2b
        integer                      :: line_fully_contains, sum_fully_contains

        sum_fully_contains = 0
        open(newunit=io, file=filename, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            call printioerror(iostat, iomsg, .true.)
        end if
        do
            read(io, '(A)', iostat=iostat, iomsg=iomsg) line
            if (iostat /= 0) then
                ! end of file or I/O error -> exit loop
                call printioerror(iostat, iomsg)
                exit
            end if
            ! debug: output line after parsing
            ! print *, line
            do i = 1, 2
                dashpos = index(line, '-')
                line(dashpos:dashpos) = ','
            end do
            read(line, *) range1a, range1b, range2a, range2b
            line_fully_contains = fully_contains(range1a, range1b, range2a, range2b)
            sum_fully_contains = sum_fully_contains + line_fully_contains
        end do
        close(io)
        solve = sum_fully_contains
    end function

end module day04a
