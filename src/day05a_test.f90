module day05a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day05a, only : solve
        implicit none

        character(len=9) :: result

        result = solve('../inputfiles/day05_example.txt')
        call assert_equals ('CMZ', result)
    end subroutine

    subroutine test_solve_input
        use day05a, only : solve
        implicit none

        character(len=9) :: result

        result = solve('../inputfiles/day05_input.txt')
        call assert_equals ('XXXXXXXXX', result)
    end subroutine

end module day05a_test
