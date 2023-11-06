program day14a_main
    use util, only : printresultline_integer
    use day14a, only : solve
    implicit none

    integer :: result

    result = solve('inputfiles/day14_example.txt')
    ! result = solve('inputfiles/day14_input.txt')
    call printresultline_integer('14a', result)
end program day14a_main
