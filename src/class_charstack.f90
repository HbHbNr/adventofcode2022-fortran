module class_charstack
    implicit none
    private

    integer, parameter :: capacity = 128

    type, public :: CharStack
        private

        character(len=capacity) :: content
        integer                 :: top
    contains
        procedure :: init  => charstack_init
        procedure :: push  => charstack_push
        procedure :: pop   => charstack_pop
        procedure :: peek  => charstack_peek
        procedure :: size  => charstack_size
        procedure :: print => charstack_print
    end type CharStack

contains

    subroutine charstack_init(this)
        implicit none

        class(CharStack) :: this

        this%content = ''
        this%top = 0
    end subroutine

    subroutine charstack_push(this, char)
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
    end subroutine

    character(len=1) function charstack_pop(this)
        implicit none

        class(CharStack) :: this

        if (this%top == 0) then
            ! stack is empty, write error to stderr
            write (0, *) 'Pop failed: stack is empty!'
            stop
        end if
        charstack_pop = this%content(this%top:this%top)
        this%content(this%top:this%top) = ' '
        this%top = this%top - 1
    end function

    character(len=1) function charstack_peek(this)
        implicit none

        class(CharStack) :: this

        if (this%top == 0) then
            ! stack is empty, write error to stderr
            write (0, *) 'Peek failed: stack is empty!'
            stop
        end if
        charstack_peek = this%content(this%top:this%top)
    end function

    integer function charstack_size(this)
        implicit none

        class(CharStack) :: this

        charstack_size = this%top
    end function

    subroutine charstack_print(this)
        implicit none

        class(CharStack), intent(in) :: this

        ! print *, this%top
        if (this%top == 0) then
            print *, ''
        else
            print *, this%content(:this%top)
        end if
    end subroutine

end module class_charstack
