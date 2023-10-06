module class_intstack
    implicit none
    private

    type, public :: IntStack
        private

        integer, allocatable :: values(:)
        integer              :: top
    contains
        procedure :: init  => intstack_init
        procedure :: push  => intstack_push
        procedure :: pop   => intstack_pop
        procedure :: peek  => intstack_peek
        procedure :: size  => intstack_size
        procedure :: print => intstack_print
    end type IntStack

contains

    subroutine intstack_init(this, stacksize)
        implicit none

        class(IntStack), intent(inout) :: this
        integer, intent(in)            :: stacksize

        allocate(integer :: this%values(stacksize))
        ! initialize the string with blanks
        this%values(:) = 0
        this%top = 0
    end subroutine

    subroutine intstack_push(this, value)
        implicit none

        class(IntStack), intent(inout) :: this
        integer, intent(in)            :: value

        if (this%top == size(this%values)) then
            ! stack is full, write error to stderr
            write (0, *) 'Push failed: stack is full!'
            stop
        end if
        this%top = this%top + 1
        this%values(this%top:this%top) = value
    end subroutine

    integer function intstack_pop(this)
        implicit none

        class(IntStack), intent(inout) :: this

        if (this%top == 0) then
            ! stack is empty, write error to stderr
            write (0, *) 'Pop failed: stack is empty!'
            stop
        end if
        intstack_pop = this%values(this%top)
        this%values(this%top) = 0
        this%top = this%top - 1
    end function

    integer function intstack_peek(this)
        implicit none

        class(IntStack), intent(in) :: this

        if (this%top == 0) then
            ! stack is empty, write error to stderr
            write (0, *) 'Peek failed: stack is empty!'
            stop
        end if
        intstack_peek = this%values(this%top)
    end function

    integer function intstack_size(this)
        implicit none

        class(IntStack), intent(in) :: this

        intstack_size = this%top
    end function

    subroutine intstack_print(this)
        implicit none

        class(IntStack), intent(in) :: this

        ! print *, this%top
        if (this%top == 0) then
            print *, ''
        else
            print *, this%values(:this%top)
        end if
    end subroutine

end module class_intstack
