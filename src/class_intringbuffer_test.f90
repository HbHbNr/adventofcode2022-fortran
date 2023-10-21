module intringbuffer_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_intringbuffer
        use class_IntRingBuffer, only : IntRingBuffer
        implicit none

        type(IntRingBuffer) :: ringbuffer
        integer             :: i

        call ringbuffer%init(8)
        ! call ringbuffer%print(.true.)

        call assert_equals(.true., ringbuffer%empty())
        call ringbuffer%addLast(1)
        call ringbuffer%addLast(2)
        ! call ringbuffer%print(.true.)

        call assert_equals(.false., ringbuffer%empty())
        call assert_equals(2, ringbuffer%length())
        call ringbuffer%addLast(3)
        call ringbuffer%addLast(4)
        ! call ringbuffer%print(.true.)

        call assert_equals(4, ringbuffer%length())
        i = ringbuffer%getFirst()
        call assert_equals(1, i)
        ! call ringbuffer%print(.true.)

        call ringbuffer%addFirst(40)
        ! call ringbuffer%print(.true.)

        call ringbuffer%addFirst(39)
        ! call ringbuffer%print(.true.)

        call assert_equals(6, ringbuffer%length())
        call ringbuffer%addLast(5)
        call assert_equals(7, ringbuffer%length())
        i = ringbuffer%getFirst()
        call assert_equals(39, i)
        ! call ringbuffer%print(.true.)

        call ringbuffer%addFirst(38)
        call assert_equals(8, ringbuffer%length())
        ! call ringbuffer%print(.true.)

        i = ringbuffer%getLast()
        call assert_equals(5, i)
        i = ringbuffer%removeFirst()
        call assert_equals(38, i)
        call assert_equals(7, ringbuffer%length())
        ! call ringbuffer%print(.true.)

        i = ringbuffer%removeLast()
        ! call ringbuffer%print(.true.)

        call assert_equals(5, i)
        call assert_equals(6, ringbuffer%length())
        i = ringbuffer%removeFirst()
        call assert_equals(39, i)
        call assert_equals(5, ringbuffer%length())
        ! call ringbuffer%print(.true.)
    end subroutine

end module intringbuffer_test
