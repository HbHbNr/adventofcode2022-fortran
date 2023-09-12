module day04a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_fully_contains
        use day04a, only : fully_contains
        implicit none

        call assert_equals (0, fully_contains(2, 4, 6, 8))
        call assert_equals (0, fully_contains(2, 3, 4, 5))
        call assert_equals (0, fully_contains(5, 7, 7, 9))
        call assert_equals (1, fully_contains(2, 8, 3, 7))
        call assert_equals (1, fully_contains(6, 6, 4, 6))
        call assert_equals (0, fully_contains(2, 6, 4, 8))
    end subroutine

    subroutine test_solve_example
        use day04a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day04_example.txt')
        call assert_equals (2, result)
    end subroutine

    subroutine test_solve_input
        use day04a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day04_input.txt')
        call assert_equals (556, result)
    end subroutine

end module day04a_test
