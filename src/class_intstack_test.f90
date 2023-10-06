module charstack_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_charstack
        use class_IntStack, only : IntStack
        implicit none

        type(IntStack)  :: stack
        integer :: value

        call stack%init(8)
        call stack%push(1)
        call stack%push(2)
        call stack%push(3)
        call assert_equals(3, stack%size())
        value = stack%peek()
        call assert_equals (3, value)
        value = stack%pop()
        call assert_equals(2, stack%size())
        call assert_equals (3, value)
        value = stack%pop()
        call assert_equals (2, value)
        value = stack%pop()
        call assert_equals (1, value)
        call assert_equals(0, stack%size())
        call stack%push(1)
        call stack%push(2)
        call stack%push(3)
        call stack%push(4)
        call assert_equals(4, stack%size())
        value = stack%pop()
        call assert_equals (4, value)
        value = stack%pop()
        value = stack%pop()
        value = stack%peek()
        call assert_equals (1, value)
        call assert_equals(1, stack%size())
    end subroutine

end module charstack_test
