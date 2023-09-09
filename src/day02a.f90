!> Solution for https://adventofcode.com/2021/day/2 part a
module day02a
    implicit none
    private

    public :: round_score
    public :: solve

contains

    integer function round_score(line)
        implicit none

        character(len=3), intent(in) :: line
        character(len=1)             :: mychoice
        integer                      :: score

        ! draw:  I win:  I lose:
        ! A X    A Y     A Z
        ! B Y    B Z     B X
        ! C Z    C X     C Y
        if (line == 'A X' .or. line == 'B Y' .or. line == 'C Z') then
            ! draw
            score = 3
        else if (line == 'A Y' .or. line == 'B Z' .or. line == 'C X') then
            ! win
            score = 6
        else
            ! lose
            score = 0
        end if
        ! get mychoice from third column
        read(line, '(2X, A1)') mychoice
        if (mychoice == 'X') then
            score = score + 1
        else if (mychoice == 'Y') then
            score = score + 2
        else
            score = score + 3
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

end module day02a
