program day18b_main
    use util, only : printresultline_integer
    use day18b, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day18_example.txt')
    result = solve('inputfiles/day18_input.txt')
    call printresultline_integer('18b', result)
end program day18b_main
