!> Solution for https://adventofcode.com/2022/day/3 part a
module day03a
    use util, only : printioerror
    implicit none
    private

    public :: char_value
    public :: find_duplicate
    public :: solve

contains

    integer function char_value(char)
        implicit none

        character(len=1), intent(in) :: char

        select case(char)
        case ('a':'z')
            char_value = ichar(char) - ichar('a') + 1
        case ('A':'Z')
            char_value = ichar(char) - ichar('A') + 1 + 26
        end select
    end function

    character(len=1) function find_duplicate(rucksack)
        implicit none

        character(len=*), intent(in) :: rucksack
        integer                      :: length, halflength, i, j
        character(len=1)             :: char1, char2

        length = len_trim(rucksack)
        halflength = length / 2
        ! iterate characters from the first half a search each in the second half
        do i = 1, halflength
            char1 = rucksack(i:i)
            do j = halflength + 1, length
                char2 = rucksack(j:j)
                if (char1 == char2) then
                    find_duplicate = char1
                    exit
                end if
            end do
        end do
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in) :: filename
        integer                      :: io, iostat
        character(len=512)           :: iomsg
        character(len=64)            :: line
        integer                      :: priority, prioritysum
        character(len=1)             :: duplicate

        prioritysum = 0
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
            ! debug: output line from file and its length
            ! print '(A, A2, I1)', line, ': ', len_trim(line)
            duplicate = find_duplicate(line)
            priority = char_value(duplicate)
            prioritysum = prioritysum + priority
        end do
        close(io)

        solve = prioritysum
    end function

end module day03a
