program day07a_main
    use util, only : printresultline_integer
    use day07a, only : solve
    implicit none

    integer :: result

    result = solve('inputfiles/day07_example.txt')
    ! result = solve('inputfiles/day07_input.txt')
    call printresultline_integer('07a', result)
end program day07a_main
