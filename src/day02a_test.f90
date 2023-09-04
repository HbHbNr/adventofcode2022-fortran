program day02a_test
    use util, only : printresultline_integer
    use day02a, only : scan_strategy
    implicit none

    integer :: result
    integer :: test

    ! result = scan_strategy('inputfiles/day02_example.txt')
    result = scan_strategy('inputfiles/day02_input.txt')
    test = merge(1, 0, result == 13052)
    call printresultline_integer('02a', test)
end program day02a_test
