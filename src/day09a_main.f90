program day09a_main
    use util, only : printresultline_integer
    use day09a, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day09_example1.txt')
    result = solve('inputfiles/day09_input.txt')
    call printresultline_integer('09a', result)
end program day09a_main
