module day18a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day18a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day18_example.txt')
        call assert_equals (64, result)
    end subroutine

    subroutine test_solve_input
        use day18a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day18_input.txt')
        call assert_equals (4636, result)
    end subroutine

end module day18a_test
