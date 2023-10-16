!> Solution for https://adventofcode.com/2021/day/11 part a
module day11a
    use util, only : printioerror, readinputfile_asstringarray
    use class_intringbuffer, only : IntRingBuffer
    implicit none
    private

    public :: solve

    type Monkey
        private

        integer             :: id
        type(IntRingBuffer) :: items
        character(len=1)    :: operation
        integer             :: operation_factor
        integer             :: test_divisor
        integer             :: monkey_true_target
        integer             :: monkey_false_target
    end type Monkey

contains

    subroutine configure_monkey(themonkey, notes)
        implicit none

        type(Monkey), intent(inout)   :: themonkey
        character(len=*), intent(in)  :: notes(:)
        integer                       :: i, item
        character(len=len(notes(1)))  :: tmpstring

        ! prepare ringbuffer up to 36 items (maximum in puzzle input)
        call themonkey%items%init(36)

        read(notes(1)(8:8), *) themonkey%id
        print *, 'Configuring monkey ', themonkey%id

        read(notes(2), '(18X,A)') tmpstring
        ! items always have two digits plus command and space
        do i = 1, len_trim(tmpstring), 4
            read(tmpstring(i:i+1), *) item
            call themonkey%items%addLast(item)
        end do
        print *, 'itemliststring: ', trim(tmpstring)

        read(notes(3)(24:24), *) themonkey%operation
        read(notes(3)(26:), *) tmpstring
        if (tmpstring == 'old') then
            ! store 'old' as -1, because real factors are all above 0
            themonkey%operation_factor = -1
        else
            read(tmpstring, *) themonkey%operation_factor
        end if
        print *, 'operation: ', themonkey%operation, ' ', themonkey%operation_factor

        read(notes(4)(22:), *) themonkey%test_divisor
        print *, 'test_divisor: ', themonkey%test_divisor

        read(notes(5)(30:), *) themonkey%monkey_true_target
        print *, 'monkey_true_target: ', themonkey%monkey_true_target

        read(notes(6)(31:), *) themonkey%monkey_false_target
        print *, 'monkey_false_target: ', themonkey%monkey_false_target

    end subroutine

    function configure_monkeys(notes) result(monkeys)
        implicit none

        character(len=*), intent(in) :: notes(:)
        type(Monkey), allocatable    :: monkeys(:)
        integer                      :: number_of_monkeys, i

        number_of_monkeys = (size(notes) + 1) / 7
        ! print *, number_of_monkeys
        allocate(monkeys(0:number_of_monkeys-1))
        do i = 0, number_of_monkeys-1
            ! print *, i
            call configure_monkey(monkeys(i), notes(i*7+1:i*7+6))
        end do
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: notes(:)
        type(Monkey), allocatable     :: monkeys(:)

        ! read list of notes from file
        notes = readinputfile_asstringarray(filename, 64)

        ! evaluate list of notes and configure monkeys
        monkeys = configure_monkeys(notes)

        ! return number of visited positions
        solve = -1
    end function solve

end module day11a
