!> Solution for https://adventofcode.com/2021/day/3 part b
module day03b
    use util, only : printioerror
    implicit none
    private

    public :: char_value
    public :: find_badge
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

    subroutine find_items(line, itemlist)
        implicit none

        character(len=*), intent(in)        :: line
        integer, dimension(52), intent(out) :: itemlist
        integer                             :: i, value1
        character(len=1)                    :: char1

        itemlist = 0  ! set all values in the array to 0
        do i = 1, len_trim(line)
            char1 = line(i:i)
            value1 = char_value(char1)
            itemlist(value1) = 1
        end do
    end subroutine

    integer function find_badge(line1, line2, line3)
        implicit none

        character(len=*), intent(in) :: line1, line2, line3
        integer, dimension(52)       :: itemlist1, itemlist2, itemlist3, itemlistgroup
        integer                      :: i

        call find_items(line1, itemlist1)
        call find_items(line2, itemlist2)
        call find_items(line3, itemlist3)
        ! create a new itemlist as the sum of the three itemlist
        itemlistgroup = itemlist1 + itemlist2 + itemlist3
        ! find first appereance of "3", which is the item which is in all three itemlists
        find_badge = -1
        do i = 1, 52
            if (itemlistgroup(i) == 3) then
                find_badge = i
                exit
            end if
        end do
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in) :: filename
        integer                      :: io, iostat
        character(len=512)           :: iomsg
        character(len=64)            :: line1, line2, line3
        integer                      :: priority, prioritysum

        prioritysum = 0
        open(newunit=io, file=filename, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            call printioerror(iostat, iomsg, .true.)
        end if
        do
            read(io, '(A)', iostat=iostat, iomsg=iomsg) line1
            if (iostat /= 0) then
                ! end of file or I/O error -> exit loop
                call printioerror(iostat, iomsg)
                exit
            end if
            read(io, '(A)', iostat=iostat) line2
            read(io, '(A)', iostat=iostat) line3
            ! debug: output line from file and its length
            ! print '(A, A2, I1)', line, ': ', len_trim(line)
            priority = find_badge(line1, line2, line3)
            prioritysum = prioritysum + priority
        end do
        close(io)
        ! return maximum calories
        solve = prioritysum
    end function

end module day03b
