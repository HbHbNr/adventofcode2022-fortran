module day06a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_unique_quartet
        use day06a, only : unique_quartet
        implicit none

        call assert_equals (.true., unique_quartet('abcd'))
        call assert_equals (.false., unique_quartet('abad'))
    end subroutine

    subroutine test_solve_example
        use day06a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day06_example.txt')
        call assert_equals (11, result)
    end subroutine

    subroutine test_solve_input
        use day06a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day06_input.txt')
        call assert_equals (1262, result)
    end subroutine

end module day06a_test
