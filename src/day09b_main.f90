program day09b_main
    use util, only : printresultline_integer
    use day09b, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day09_example1.txt')
    result = solve('inputfiles/day09_example2.txt')
    ! result = solve('inputfiles/day09_input.txt')
    call printresultline_integer('09b', result)
end program day09b_main
