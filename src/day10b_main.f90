program day10b_main
    use util, only : printresultline_integer
    use day10b, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day10_example1.txt')
    ! result = solve('inputfiles/day10_example2.txt')
    result = solve('inputfiles/day10_input.txt')
    call printresultline_integer('10b', result)
    ! decoded result: RKPJBPLA
end program day10b_main
