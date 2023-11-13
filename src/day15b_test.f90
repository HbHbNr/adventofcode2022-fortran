module day15b_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day15b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day15_example.txt', 20)
        call assert_equals (-1, result)
    end subroutine

    subroutine test_solve_input
        use day15b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day15_input.txt', 4000000)
        call assert_equals (-1, result)
    end subroutine

end module day15b_test
