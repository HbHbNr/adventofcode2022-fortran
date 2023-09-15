module class_CharStack
    implicit none
    private

    public :: charstack_push
    public :: charstack_print

    integer, parameter :: capacity = 32

    type, public :: CharStack
        character(len=capacity) :: content
        integer                 :: top
    contains
        procedure :: push  => charstack_push
        procedure :: pop   => charstack_pop
        procedure :: print => charstack_print
    end type CharStack

contains

    character(len=1) function charstack_push(this, char)
        implicit none

        class(CharStack)             :: this
        character(len=1), intent(in) :: char

        if (this%top == len(this%content)) then
            ! stack is full, write error to stderr
            write (0, *) 'Push failed: stack is full!'
            stop
        end if
        this%top = this%top + 1
        this%content(this%top:this%top) = char
        charstack_push = char
    end function

    character(len=1) function charstack_pop(this)
        implicit none

        class(CharStack) :: this

        if (this%top == 0) then
            ! stack is empty, write error to stderr
            write (0, *) 'Pop failed: stack is empty!'
            stop
        end if
        charstack_pop = this%content(this%top:this%top)
        this%top = this%top - 1
    end function

    subroutine charstack_print(this)
        implicit none

        class(CharStack), intent(in) :: this

        print *, 'start'
        print *, this%top
        if (this%top == 0) then
            print *, ''
        else
            print *, this%content(:this%top)
        end if
        print *, 'end'
    end subroutine

end module class_CharStack

program CharStack_test
    use class_CharStack  !, only : CharStack, charstack_push
    implicit none

    type(CharStack)  :: stack
    character(len=1) :: char

    stack = CharStack('ABC', 3)
    call stack%print
    char = stack%pop()
    print *, char
    call stack%print
    char = stack%pop()
    print *, char
    call stack%print
    char = stack%pop()
    print *, char
    call stack%print
    ! stack%push(char)
    ! stack%push('B')
    ! stack%push('C')
    charstack_push(stack, 'A')
    call charstack_print(stack)
end program CharStack_test
