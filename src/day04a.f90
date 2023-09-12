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

    integer function solve(filename)
        implicit none

        type :: sectionline
            ! example:
            ! 2-4,6-8
            integer :: section1a
            character(len=1) :: dash1
            integer :: section1b
            character(len=1) :: comma
            integer :: section2a
            character(len=1) :: dash2
            integer :: section2b
        end type

        character(len=*), intent(in) :: filename
        type(sectionline)            :: line
        integer                      :: io, iostat
        character(len=1000)          :: iomsg
        integer                      :: line_fully_contains, sum_fully_contains

        print *, filename
        sum_fully_contains = 0
        open(newunit=io, file=filename, status='old', action='read')
        do
            read(io, *, iostat=iostat, iomsg=iomsg) line
            if (iostat /= 0) then
                ! end of file or I/O error -> exit loop
                print *, 'error: ', iostat, iomsg
                exit
            end if
            ! debug: output line from file and its length
            print *, line
            ! print *, line%section1a, line%section1b, line%section2a, line%section2b
            line_fully_contains = fully_contains(line%section1a, line%section1b, line%section2a, line%section2b)
            sum_fully_contains = sum_fully_contains + line_fully_contains
        end do
        close(io)
        solve = sum_fully_contains
    end function

end module day04a
