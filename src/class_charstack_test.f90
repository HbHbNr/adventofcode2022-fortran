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

        stack = CharStack('ABC', 3)
        char = stack%pop()
        call assert_equals ('C', char)
        char = stack%pop()
        call assert_equals ('B', char)
        char = stack%pop()
        call assert_equals ('A', char)
        char = stack%push('A')
        char = stack%push('B')
        char = stack%push('C')
        char = stack%push('D')
        char = stack%pop()
        call assert_equals ('D', char)
    end subroutine

end module charstack_test
