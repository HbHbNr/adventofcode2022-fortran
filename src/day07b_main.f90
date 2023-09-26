program day07b_main
    use util, only : printresultline_integer
    use day07b, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day07_example.txt')
    result = solve('inputfiles/day07_input.txt')
    call printresultline_integer('07b', result)
end program day07b_main
