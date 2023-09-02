!!! Solution for https://adventofcode.com/2021/day/1 part a

integer function round_score(line)
    implicit none

    character(len=3), intent(in) :: line
    character(len=1)             :: opponent, space, me
    integer                      :: score

    ! draw:
    ! A X
    ! B Y
    ! C Z
    !
    ! I win:
    ! A Z
    ! B X
    ! C Y
    !
    ! I lose:
    ! A Y
    ! B Z
    ! C X
    if (line == 'A X' .or. line == 'B Y' .or. line == 'C Z') then
        ! draw
        print *, 'draw'
        score = 3
    else if (line == 'A Z' .or. line == 'B X' .or. line == 'C Y') then
        ! win
        print *, 'win'
        score = 6
    else
        ! lose
        print *, 'lose'
        score = 0
    end if
    read(line, '(A1, A1, A1)') opponent, space, me
    if (me == 'X') then
        score = score + 1
    else if (me == 'Y') then
        score = score + 2
    else
        score = score + 3
    end if
    ! return round score
    print *, score
    round_score = score
end function

integer function scan_strategy(filename)
    implicit none

    character(len=*), intent(in) :: filename
    character(len=3)             :: line
    integer                      :: totalscore = 0, round_score
    integer                      :: io, iostat

    open(newunit=io, file=filename, status='old', action='read')
    do
        read(io, '(A3)', iostat=iostat) line
        if (iostat /= 0) then
            ! end of file or I/O error -> exit loop
            exit
        end if
        ! debug: output line from file and its length
        print '(A3, A2, I1)', line, ': ', len_trim(line)
        totalscore = totalscore + round_score(line)
    end do
    close(io)
    ! return total score
    scan_strategy = totalscore
end function

program day02a
    use util
    implicit none

    integer :: result, scan_strategy

    ! result = scan_strategy('inputfiles/day02_example.txt')
    result = scan_strategy('inputfiles/day02_input.txt')
    call printresultline_integer('02a', result)
end program day02a
