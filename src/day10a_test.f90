module day10a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example1
        use day10a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day10_example1.txt')
        call assert_equals (0, result)
    end subroutine test_solve_example1

    subroutine test_solve_example2
        use day10a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day10_example2.txt')
        call assert_equals (13140, result)
    end subroutine test_solve_example2

    subroutine test_solve_input
        use day10a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day10_input.txt')
        call assert_equals (15120, result)
    end subroutine test_solve_input

end module day10a_test
