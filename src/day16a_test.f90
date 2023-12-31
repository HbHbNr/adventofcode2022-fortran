module day16a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day16a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day16_example.txt')
        call assert_equals (1651, result)
    end subroutine

    subroutine test_solve_input
        use day16a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day16_input.txt')
        call assert_equals (2087, result)
    end subroutine

end module day16a_test
