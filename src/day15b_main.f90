program day15b_main
    use util, only : printresultline_integer
    use day15b, only : solve
    implicit none

    integer :: result

    result = solve('inputfiles/day15_example.txt', 20)
    ! result = solve('inputfiles/day15_input.txt', 4000000)
    call printresultline_integer('15b', result)
end program day15b_main
