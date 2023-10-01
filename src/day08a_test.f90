module day08a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day08a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day08_example.txt', 5)
        call assert_equals (21, result)
    end subroutine

    subroutine test_solve_input
        use day08a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day08_input.txt', 99)
        call assert_equals (1809, result)
    end subroutine

end module day08a_test
