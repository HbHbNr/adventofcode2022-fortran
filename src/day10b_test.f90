module day10b_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example1
        use day10b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day10_example1.txt')
        call assert_equals (-1, result)
    end subroutine test_solve_example1

    subroutine test_solve_example2
        use day10b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day10_example2.txt')
        call assert_equals (-1, result)
    end subroutine test_solve_example2

    subroutine test_solve_input
        use day10b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day10_input.txt')
        call assert_equals (-1, result)
    end subroutine test_solve_input

end module day10b_test
