program day14b_main
    use util, only : printresultline_integer
    use day14b, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day14_example.txt')
    result = solve('inputfiles/day14_input.txt')
    call printresultline_integer('14b', result)
end program day14b_main
