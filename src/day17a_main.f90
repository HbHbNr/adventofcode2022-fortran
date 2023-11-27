program day17a_main
    use util, only : printresultline_integer
    use day17a, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day17_example.txt')
    result = solve('inputfiles/day17_input.txt')
    call printresultline_integer('17a', result)
end program day17a_main
