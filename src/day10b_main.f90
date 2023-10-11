program day10b_main
    use util, only : printresultline_stringarray
    use day10b, only : solve
    implicit none

    character(len=40) :: result(6)

    ! result = solve('inputfiles/day10_example2.txt')
    result = solve('inputfiles/day10_input.txt')
    call printresultline_stringarray('10b', result)
    ! decoded result: RKPJBPLA
end program day10b_main
