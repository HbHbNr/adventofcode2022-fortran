program day08b_main
    use util, only : printresultline_integer
    use day08b, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day08_example.txt', 5)
    result = solve('inputfiles/day08_input.txt', 99)
    call printresultline_integer('08b', result)
end program day08b_main
