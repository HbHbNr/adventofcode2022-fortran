program day15a_main
    use util, only : printresultline_integer
    use day15a, only : solve
    implicit none

    integer :: result

    result = solve('inputfiles/day15_example.txt')
    ! result = solve('inputfiles/day15_input.txt')
    call printresultline_integer('15a', result)
end program day15a_main
