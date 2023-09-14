!> Solution for https://adventofcode.com/2021/day/2 part a
module day02a
    use util, only : printioerror
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
        select case (line)
        case ('A X', 'B Y', 'C Z')
            ! draw
            score = 3
        case ('A Y', 'B Z', 'C X')
            ! win
            score = 6
        case default
            ! lose
            score = 0
        end select
        ! get mychoice from third column
        read(line, '(2X, A1)') mychoice
        select case(mychoice)
        case ('X')
            score = score + 1
        case ('Y')
            score = score + 2
        case default
            score = score + 3
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

end module day02a
