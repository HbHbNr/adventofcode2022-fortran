subroutine printresultline(day, result)
    implicit none

    character(len=*) :: day
    character(len=*) :: result

    print '(A, A, A, A)', 'Day ', day, ': ', adjustl(result)
end subroutine

integer function scan_calories(filename)
    implicit none

    character(len=*) :: filename
    character(len=5) :: line
    integer          :: linecalories, sumcalories, maxcalories
    integer          :: io, iostat

    open(newunit=io, file=filename, status='old', action='read')
    sumcalories = 0
    maxcalories = 0
    do
        read(io, '(A)', iostat=iostat) line
        if (iostat /= 0) then
            ! end of file or I/O error -> exit loop
            exit
        end if
        ! print '(A5, A2, I1)', line, ': ', len_trim(line)  ! debug: output line and its length
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

    integer      :: intresult, scan_calories
    character(len=10) :: result
    ! intresult = scan_calories('inputfiles/day01_example.txt')
    intresult = scan_calories('inputfiles/day01_input.txt')
    ! print *, intresult
    ! result = '42'
    write(result, '(I10)') intresult
    call printresultline('01a', result)
end program day01a
