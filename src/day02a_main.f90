program day02a_main
    use util, only : printresultline_integer
    use day02a, only : scan_strategy
    implicit none

    integer :: result

    ! result = scan_strategy('inputfiles/day02_example.txt')
    result = scan_strategy('inputfiles/day02_input.txt')
    call printresultline_integer('02a', result)
end program day02a_main
