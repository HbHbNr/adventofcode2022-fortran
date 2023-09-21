module charstack_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_charstack
        use class_CharStack, only : CharStack
        implicit none

        type(CharStack)  :: stack
        character(len=1) :: char

        call stack%init()
        call stack%push('A')
        call stack%push('B')
        call stack%push('C')
        call assert_equals(3, stack%size())
        char = stack%peek()
        call assert_equals ('C', char)
        char = stack%pop()
        call assert_equals(2, stack%size())
        call assert_equals ('C', char)
        char = stack%pop()
        call assert_equals ('B', char)
        char = stack%pop()
        call assert_equals ('A', char)
        call assert_equals(0, stack%size())
        call stack%push('A')
        call stack%push('B')
        call stack%push('C')
        call stack%push('D')
        call assert_equals(4, stack%size())
        char = stack%pop()
        call assert_equals ('D', char)
        char = stack%pop()
        char = stack%pop()
        char = stack%peek()
        call assert_equals ('A', char)
        call assert_equals(1, stack%size())
    end subroutine

end module charstack_test
