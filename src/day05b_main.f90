program day05b_main
    use util, only : printresultline
    use day05b, only : solve
    implicit none

    character(len=:), allocatable :: result

    ! result = solve('inputfiles/day05_example.txt')
    result = solve('inputfiles/day05_input.txt')
    call printresultline('05b', result)
end program day05b_main
