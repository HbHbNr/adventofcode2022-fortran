module day15b_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day15b, only : solve
        implicit none

        integer(int64) :: result
        logical        :: isequal

        result = solve('../inputfiles/day15_example.txt', 20)
        isequal = (56000011_int64 == result)
        call assert_equals (isequal, .true.)
    end subroutine

    subroutine test_solve_input
        use day15b, only : solve
        implicit none

        integer(int64) :: result
        logical        :: isequal

        result = solve('../inputfiles/day15_input.txt', 4000000)
        isequal = (10996191429555_int64 == result)
        call assert_equals (isequal, .true.)
    end subroutine

end module day15b_test
