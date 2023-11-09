module day14a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day14a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day14_example.txt')
        call assert_equals (24, result)
    end subroutine

    subroutine test_solve_input
        use day14a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day14_input.txt')
        call assert_equals (610, result)
    end subroutine

end module day14a_test
