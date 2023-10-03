module day09a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day09a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day09_example.txt')
        call assert_equals (13, result)
    end subroutine

    subroutine test_solve_input
        use day09a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day09_input.txt')
        call assert_equals (6256, result)
    end subroutine

end module day09a_test
