module day11b_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day11b, only : solve
        implicit none

        integer(int64) :: result
        logical        :: isequal

        result = solve('../inputfiles/day11_example.txt')
        isequal = (2713310158_int64 == result)
        call assert_equals (isequal, .true.)
    end subroutine

    subroutine test_solve_input
        use day11b, only : solve
        implicit none

        integer(int64) :: result
        logical        :: isequal

        result = solve('../inputfiles/day11_input.txt')
        isequal = (17408399184_int64 == result)
        call assert_equals (isequal, .true.)
    end subroutine

end module day11b_test
