program day06b_main
    use util, only : printresultline_integer
    use day06b, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day06_example.txt')
    result = solve('inputfiles/day06_input.txt')
    call printresultline_integer('06b', result)
end program day06b_main
