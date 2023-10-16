module day11a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day11a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day11_example.txt')
        call assert_equals (-1, result)
    end subroutine

    subroutine test_solve_input
        use day11a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day11_input.txt')
        call assert_equals (-1, result)
    end subroutine

end module day11a_test
