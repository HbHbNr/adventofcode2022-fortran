!> Solution for https://adventofcode.com/2021/day/4 part a
module day04a
    implicit none
    private

    public :: fully_contains
    public :: solve

contains

    integer function fully_contains(section1a, section1b, section2a, section2b)
        implicit none

        integer, intent(in) :: section1a, section1b, section2a, section2b

        if (((section1a <= section2a) .and. (section1b >= section2b)) &
            .or. &
            ((section1a >= section2a) .and. (section1b <= section2b))) then
            fully_contains = 1
        else
            fully_contains = 0
        end if
    end function

    subroutine printioerror(iostat, iomsg, alwaysstop)
        implicit none

        integer, intent(in)           :: iostat
        character(len=*), intent(in)  :: iomsg
        logical, intent(in), optional :: alwaysstop
        character(len=80)             :: iostatstr

        if (iostat > 0) then
            write (iostatstr, *) iostat
            print '(A, A, A, A, A)', 'I/O error: ', trim(adjustl(iostatstr)), ' (', trim(iomsg), ')'
        end if
        if ((iostat > 0) .or. (present(alwaysstop) .and. alwaysstop)) then
            stop
        end if
    end subroutine

    integer function solve(filename)
        implicit none

        character(len=*), intent(in) :: filename
        character(len=5)             :: sections1, sections2
        integer                      :: dashpos, section1a, section1b, section2a, section2b
        integer                      :: io, iostat
        character(len=512)           :: iomsg
        integer                      :: line_fully_contains, sum_fully_contains

        sum_fully_contains = 0
        open(newunit=io, file=filename, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            call printioerror(iostat, iomsg, .true.)
        end if
        do
            read(io, *, iostat=iostat, iomsg=iomsg) sections1, sections2
            if (iostat /= 0) then
                ! end of file or I/O error -> exit loop
                call printioerror(iostat, iomsg)
                exit
            end if
            ! debug: output line after parsing
            ! print *, sections1, ',', sections2
            dashpos = index(sections1, '-')
            sections1(dashpos:dashpos) = ','
            dashpos = index(sections2, '-')
            sections2(dashpos:dashpos) = ','
            read(sections1, *) section1a, section1b
            read(sections2, *) section2a, section2b
            line_fully_contains = fully_contains(section1a, section1b, section2a, section2b)
            sum_fully_contains = sum_fully_contains + line_fully_contains
        end do
        close(io)
        solve = sum_fully_contains
    end function

end module day04a
