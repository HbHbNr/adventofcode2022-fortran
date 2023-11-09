module day15a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day15a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day15_example.txt')
        call assert_equals (-1, result)
    end subroutine

    subroutine test_solve_input
        use day15a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day15_input.txt')
        call assert_equals (-1, result)
    end subroutine

end module day15a_test
