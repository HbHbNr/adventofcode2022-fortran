program day13a_main
    use util, only : printresultline_integer
    use day13a, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day13_example.txt')
    result = solve('inputfiles/day13_input.txt')
    call printresultline_integer('13a', result)
end program day13a_main
