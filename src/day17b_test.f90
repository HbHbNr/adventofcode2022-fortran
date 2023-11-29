module day17b_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day17b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day17_example.txt')
        ! call assert_equals (3068, result)
        call assert_equals (-1, result)
    end subroutine

    subroutine test_solve_input
        use day17b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day17_input.txt')
        ! call assert_equals (3211, result)
        call assert_equals (-1, result)
    end subroutine

end module day17b_test
