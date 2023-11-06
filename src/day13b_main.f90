program day13b_main
    use util, only : printresultline_integer
    use day13b, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day13_example.txt')
    result = solve('inputfiles/day13_input.txt')
    call printresultline_integer('13b', result)
end program day13b_main
