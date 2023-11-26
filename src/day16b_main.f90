program day16b_main
    use util, only : printresultline_integer
    use day16b, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day16_example.txt')
    result = solve('inputfiles/day16_input.txt')
    call printresultline_integer('16b', result)
end program day16b_main
