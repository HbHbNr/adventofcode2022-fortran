module day09b_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example1
        use day09b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day09_example1.txt')
        call assert_equals (1, result)
    end subroutine

    subroutine test_solve_example2
        use day09b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day09_example2.txt')
        call assert_equals (36, result)
    end subroutine

    subroutine test_solve_input
        use day09b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day09_input.txt')
        call assert_equals (2665, result)
    end subroutine

end module day09b_test
