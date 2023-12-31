module day15a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day15a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day15_example.txt', 10)
        call assert_equals (26, result)
    end subroutine

    subroutine test_solve_input
        use day15a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day15_input.txt', 2000000)
        call assert_equals (6425133, result)
    end subroutine

end module day15a_test
