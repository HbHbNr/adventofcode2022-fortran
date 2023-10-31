program day12b_main
    use util, only : printresultline_integer
    use day12b, only : solve
    implicit none

    integer :: result

    result = solve('inputfiles/day12_example.txt')
    ! result = solve('inputfiles/day12_input.txt')
    call printresultline_integer('12b', result)
end program day12b_main
