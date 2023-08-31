!!! Solution for https://adventofcode.com/2021/day/1 part a

subroutine printresultline_integer(day, intresult)
    implicit none

    character(len=*), intent(in) :: day
    integer(kind=4) , intent(in) :: intresult
    character(len=11)            :: result

    write(result, '(I11)') intresult
    call printresultline(day, result)
end subroutine

subroutine printresultline(day, result)
    implicit none

    character(len=*), intent(in) :: day
    character(len=*), intent(in) :: result

    print '(A, A, A, A)', 'Day ', day, ': ', adjustl(result)
end subroutine

integer function scan_calories(filename)
    implicit none

    character(len=*), intent(in) :: filename
    character(len=5)             :: line
    integer                      :: linecalories, sumcalories = 0, maxcalories = 0
    integer                      :: io, iostat

    open(newunit=io, file=filename, status='old', action='read')
    do
        read(io, '(A)', iostat=iostat) line
        if (iostat /= 0) then
            ! end of file or I/O error -> exit loop
            exit
        end if
        ! debug: output line from file and its length
        ! print '(A5, A2, I1)', line, ': ', len_trim(line)
        if (len_trim(line) == 0) then
            ! reset sum for a new Elf
            sumcalories = 0
        else
            ! same Elf, so add calories to the sum
            read(line, *) linecalories
            sumcalories = sumcalories + linecalories
            maxcalories = max(maxcalories, sumcalories)
        end if
    end do
    close(io)
    ! return maximum calories
    scan_calories = maxcalories
end function

program day01a
    implicit none

    integer :: intresult, scan_calories

    ! intresult = scan_calories('inputfiles/day01_example.txt')
    intresult = scan_calories('inputfiles/day01_input.txt')
    call printresultline_integer('01a', intresult)
end program day01a
