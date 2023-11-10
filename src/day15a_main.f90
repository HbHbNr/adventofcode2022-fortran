program day15a_main
    use util, only : printresultline_integer
    use day15a, only : solve
    implicit none

    integer :: result

    result = solve('inputfiles/day15_example.txt', 10)
    ! result = solve('inputfiles/day15_input.txt', 2000000)
    call printresultline_integer('15a', result)
end program day15a_main
