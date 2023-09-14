!> Solution for https://adventofcode.com/2021/day/2 part b
module day02b
    use util, only : printioerror
    implicit none
    private

    public :: round_score
    public :: solve

contains

    integer function round_score(line)
        implicit none

        character(len=3), intent(in) :: line
        character(len=1)             :: hiscoice, mychoice
        integer                      :: score

        ! draw:  I win:  I lose:
        ! A Y    A Z     A X
        ! B Y    B Z     B X
        ! C Y    C Z     C X
        ! get our choices from first and third column
        read(line, '(A1, 1X, A1)') hiscoice, mychoice

        select case (mychoice)
        case ('Y')
            ! draw
            score = 3
            select case (hiscoice)
            case ('A')
                score = score + 1
            case ('B')
                score = score + 2
            case default
                score = score + 3
            end select
        case ('Z')
            ! win
            score = 6
            select case (hiscoice)
            case ('A')
                score = score + 2
            case ('B')
                score = score + 3
            case default
                score = score + 1
            end select
        case default
            ! lose
            score = 0
            select case (hiscoice)
            case ('A')
                score = score + 3
            case ('B')
                score = score + 1
            case default
                score = score + 2
            end select
        end select
        ! return round score
        round_score = score
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in) :: filename
        integer                      :: io, iostat
        character(len=512)           :: iomsg
        character(len=3)             :: line
        integer                      :: totalscore

        totalscore = 0
        open(newunit=io, file=filename, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            call printioerror(iostat, iomsg, .true.)
        end if
        do
            read(io, '(A3)', iostat=iostat, iomsg=iomsg) line
            if (iostat /= 0) then
                ! end of file or I/O error -> exit loop
                call printioerror(iostat, iomsg)
                exit
            end if
            ! debug: output line from file and its length
            ! print '(A3, A2, I1)', line, ': ', len_trim(line)
            totalscore = totalscore + round_score(line)
        end do
        close(io)
        ! return total score
        solve = totalscore
    end function

end module day02b
