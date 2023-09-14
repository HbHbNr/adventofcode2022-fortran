!> Solution for https://adventofcode.com/2021/day/4 part b
module day04b
    use util, only : printioerror
    implicit none
    private

    public :: overlap
    public :: solve

contains

    integer function overlap(range1a, range1b, range2a, range2b)
        implicit none

        integer, intent(in) :: range1a, range1b, range2a, range2b

        ! 3,85,2,86             0
        ! 10,10,9,28            0

        if (((range1a <= range2a) .and. (range1b >= range2a)) &
            .or. &
            ((range1a <= range2b) .and. (range1b >= range2b)) &
            .or. &
            ((range1a >= range2a) .and. (range1b <= range2b))) then
            overlap = 1
        else
            overlap = 0
        end if
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in) :: filename
        character(len=11)            :: line
        integer                      :: io, iostat
        character(len=512)           :: iomsg
        integer                      :: i, dashpos, range1a, range1b, range2a, range2b
        integer                      :: line_overlap, sum_overlap

        sum_overlap = 0
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
            line_overlap = overlap(range1a, range1b, range2a, range2b)
            ! print *, line, line_overlap
            sum_overlap = sum_overlap + line_overlap
        end do
        close(io)
        solve = sum_overlap
    end function

end module day04b
