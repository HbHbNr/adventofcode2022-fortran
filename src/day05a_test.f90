module day05a_test
    use fruit
    use class_CharStack, only : CharStack
    use day05a, only : init_stacks
    implicit none

    ! all tests will be public

contains

    subroutine test_init_stacks()
        implicit none

        character(len=64) :: lines(3) = (/ '    [D]    ', '[N] [C]    ', '[Z] [M] [P]' /)
        type(CharStack)   :: stacks(3)

        ! stacks will be initialized in init_stacks
        call init_stacks(lines, stacks)
        call assert_equals('N', stacks(1)%pop())
        call assert_equals('Z', stacks(1)%pop())
        call assert_equals('D', stacks(2)%pop())
        call assert_equals('C', stacks(2)%pop())
        call assert_equals('M', stacks(2)%pop())
        call assert_equals('P', stacks(3)%pop())
    end subroutine

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
        call assert_equals ('VJSFHWGFT', result)
    end subroutine

end module day05a_test
