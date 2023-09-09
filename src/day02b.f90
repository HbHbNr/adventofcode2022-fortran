!> Solution for https://adventofcode.com/2021/day/2 part b
module day02b
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

        if (mychoice == 'Y') then
            ! draw
            score = 3
            if (hiscoice == 'A') then
                score = score + 1
            else if (hiscoice == 'B') then
                score = score + 2
            else
                score = score + 3
            end if
        else if (mychoice == 'Z') then
            ! win
            score = 6
            if (hiscoice == 'A') then
                score = score + 2
            else if (hiscoice == 'B') then
                score = score + 3
            else
                score = score + 1
            end if
        else
            ! lose
            score = 0
            if (hiscoice == 'A') then
                score = score + 3
            else if (hiscoice == 'B') then
                score = score + 1
            else
                score = score + 2
            end if
        end if
        ! return round score
        round_score = score
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in) :: filename
        character(len=3)             :: line
        integer                      :: io, iostat
        integer                      :: totalscore

        totalscore = 0
        open(newunit=io, file=filename, status='old', action='read')
        do
            read(io, '(A3)', iostat=iostat) line
            if (iostat /= 0) then
                ! end of file or I/O error -> exit loop
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
