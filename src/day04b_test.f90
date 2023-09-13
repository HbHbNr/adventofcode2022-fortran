module day04b_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_overlap
        use day04b, only : overlap
        implicit none

        call assert_equals (0, overlap(2, 4, 6, 8))
        call assert_equals (0, overlap(2, 3, 4, 5))
        call assert_equals (1, overlap(5, 7, 7, 9))
        call assert_equals (1, overlap(2, 8, 3, 7))
        call assert_equals (1, overlap(6, 6, 4, 6))
        call assert_equals (1, overlap(2, 6, 4, 8))
    end subroutine

    subroutine test_solve_example
        use day04b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day04_example.txt')
        call assert_equals (4, result)
    end subroutine

    subroutine test_solve_input
        use day04b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day04_input.txt')
        call assert_equals (876, result)
    end subroutine

end module day04b_test
