program day11a_main
    use util, only : printresultline_integer
    use day11a, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day11_example.txt')
    result = solve('inputfiles/day11_input.txt')
    call printresultline_integer('11a', result)
end program day11a_main
