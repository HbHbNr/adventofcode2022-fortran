program day20a_main
    use util, only : printresultline_integer
    use day20a, only : solve
    implicit none

    integer :: result

    result = solve('inputfiles/day20_example.txt')
    ! result = solve('inputfiles/day20_input.txt')
    call printresultline_integer('20a', result)
end program day20a_main
