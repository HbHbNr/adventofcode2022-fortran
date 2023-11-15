program day16a_main
    use util, only : printresultline_integer
    use day16a, only : solve
    implicit none

    integer :: result

    result = solve('inputfiles/day16_example.txt')
    ! result = solve('inputfiles/day16_input.txt')
    call printresultline_integer('16a', result)
end program day16a_main
