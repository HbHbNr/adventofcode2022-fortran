program day12a_main
    use util, only : printresultline_integer
    use day12a, only : solve
    implicit none

    integer :: result

    result = solve('inputfiles/day12_example.txt')
    ! result = solve('inputfiles/day12_input.txt')
    call printresultline_integer('12a', result)
end program day12a_main
