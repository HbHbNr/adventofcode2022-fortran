module day01a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day01a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day01_example.txt')
        call assert_equals (24000, result)
    end subroutine

    subroutine test_solve_input
        use day01a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day01_input.txt')
        call assert_equals (69528, result)
    end subroutine

end module day01a_test
