program day19a_main
    use util, only : printresultline_integer
    use day19a, only : solve
    implicit none

    integer :: result

    result = solve('inputfiles/day19_example.txt')
    ! result = solve('inputfiles/day19_input.txt')
    call printresultline_integer('19a', result)
end program day19a_main
