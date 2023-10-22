module day12a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day12a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day12_example.txt')
        call assert_equals (-1, result)
    end subroutine

    subroutine test_solve_input
        use day12a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day12_input.txt')
        call assert_equals (-1, result)
    end subroutine

end module day12a_test
