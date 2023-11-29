program day17b_main
    use util, only : printresultline_integer
    use day17b, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day17_example.txt')
    result = solve('inputfiles/day17_input.txt')
    call printresultline_integer('17b', result)
end program day17b_main
