program day05a_main
    use util, only : printresultline
    use day05a, only : solve
    implicit none

    character(len=9) :: result

    ! result = solve('inputfiles/day05_example.txt')
    result = solve('inputfiles/day05_input.txt')
    call printresultline('05a', result)
end program day05a_main
