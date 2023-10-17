program day11b_main
    use util, only : printresultline_integer
    use day11b, only : solve
    implicit none

    integer :: result

    result = solve('inputfiles/day11_example.txt')
    ! result = solve('inputfiles/day11_input.txt')
    call printresultline_integer('11b', result)
end program day11b_main
