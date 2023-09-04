module day02a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_scan_strategy_example
        use day02a, only : scan_strategy
        implicit none

        integer :: result

        result = scan_strategy('inputfiles/day02_example.txt')
        call assert_equals (15, result)
    end subroutine

    subroutine test_scan_strategy_input
        use day02a, only : scan_strategy
        implicit none

        integer :: result

        result = scan_strategy('inputfiles/day02_input.txt')
        call assert_equals (13052, result)
    end subroutine

end module day02a_test
