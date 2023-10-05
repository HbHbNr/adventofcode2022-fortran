program day10a_main
    use util, only : printresultline_integer
    use day10a, only : solve
    implicit none

    integer :: result

    result = solve('inputfiles/day10_example1.txt')
    ! result = solve('inputfiles/day10_example2.txt')
    ! result = solve('inputfiles/day10_input.txt')
    call printresultline_integer('10a', result)
end program day10a_main
