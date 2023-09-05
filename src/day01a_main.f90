program day01a_main
    use util, only : printresultline_integer
    use day01a, only : scan_calories
    implicit none

    integer :: result

    ! result = scan_calories('inputfiles/day01_example.txt')
    result = scan_calories('inputfiles/day01_input.txt')
    call printresultline_integer('01a', result)
end program day01a_main
