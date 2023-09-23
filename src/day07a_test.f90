module day07a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day07a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day07_example.txt')
        call assert_equals (-1, result)
    end subroutine

    subroutine test_solve_input
        use day07a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day07_input.txt')
        call assert_equals (-1, result)
    end subroutine

end module day07a_test
