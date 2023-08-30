subroutine printresultline(day, result)
    implicit none

    character(len=*) :: day
    character(len=*) :: result

    print '(A, A, A, A)', 'Day ', day, ': ', result
end subroutine

program day01a
    implicit none

    character(2) :: result
    result = '42'
    call printresultline('01a', result)
end program day01a
