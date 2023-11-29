program day18a_main
    use util, only : printresultline_integer
    use day18a, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day18_example.txt')
    result = solve('inputfiles/day18_input.txt')
    call printresultline_integer('18a', result)
end program day18a_main
