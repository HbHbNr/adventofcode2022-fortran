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
        integer             :: total_inspections
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

        themonkey%total_inspections = 0
    end subroutine

    subroutine configure_monkeys(notes, monkeys)
        implicit none

        character(len=*), intent(in) :: notes(:)
        type(Monkey), allocatable, intent(inout)    :: monkeys(:)
        integer                      :: number_of_monkeys, i

        number_of_monkeys = (size(notes) + 1) / 7
        ! print *, number_of_monkeys
        allocate(monkeys(0:number_of_monkeys-1))
        print *, 'configure_monkeys monkey bounds: ', lbound(monkeys, 1), ubound(monkeys, 1)
        do i = 0, number_of_monkeys-1
            ! print *, i
            call configure_monkey(monkeys(i), notes(i*7+1:i*7+6))
        end do
    end subroutine

    subroutine calculate_monkey(activemonkeyid, monkeys)
        implicit none

        integer, intent(in)       :: activemonkeyid
        type(Monkey), allocatable :: monkeys(:)
        integer                   :: item, operation_factor
        logical                   :: test_result

        do while(.not. monkeys(activemonkeyid)%items%empty())
            ! take first item from list
            item = monkeys(activemonkeyid)%items%removeFirst()
            monkeys(activemonkeyid)%total_inspections = monkeys(activemonkeyid)%total_inspections + 1

            ! set the operation factor - -1 means 'old'
            operation_factor = monkeys(activemonkeyid)%operation_factor
            if (operation_factor == -1) operation_factor = item

            ! perform operation based on operator
            select case(monkeys(activemonkeyid)%operation)
            case ('+')
                item = item + operation_factor
            case ('*')
                item = item * operation_factor
            end select

            ! divide worry level by three
            item = item / 3

            ! test if item is divisible by requested factor
            test_result = (modulo(item, monkeys(activemonkeyid)%test_divisor) == 0)
            if (test_result) then
                call monkeys(monkeys(activemonkeyid)%monkey_true_target)%items%addLast(item)
            else
                call monkeys(monkeys(activemonkeyid)%monkey_false_target)%items%addLast(item)
            end if
        end do
    end subroutine

    subroutine calculate_round(round, monkeys)
        implicit none

        integer, intent(in)       :: round
        type(Monkey), allocatable :: monkeys(:)
        integer                   :: activemonkeyid

        print *, 'calculate_round monkey bounds: ', lbound(monkeys, 1), ubound(monkeys, 1)

        print *, 'Calculate round', round
        do activemonkeyid = lbound(monkeys, 1), ubound(monkeys, 1)
            call calculate_monkey(activemonkeyid, monkeys)
        end do

        print *, 'Round results:', round
        do activemonkeyid = lbound(monkeys, 1), ubound(monkeys, 1)
            call monkeys(activemonkeyid)%items%print()
        end do
    end subroutine

    function calculate_monkeybusiness(monkeys) result(monkeybusiness)
        implicit none

        type(Monkey), intent(in) :: monkeys(:)
        integer                  :: monkeybusiness
        integer                  :: i, first, second, current

        first = 0
        second = 0
        do i = lbound(monkeys, 1), ubound(monkeys, 1)
            current = monkeys(i)%total_inspections
            if (current > first) then
                second = first
                first = current
            else if (current > second) then
                second = current
            end if
        end do

        monkeybusiness = first * second
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: notes(:)
        type(Monkey), allocatable     :: monkeys(:)
        integer                       :: round

        ! read list of notes from file
        notes = readinputfile_asstringarray(filename, 64)

        ! evaluate list of notes and configure monkeys
        call configure_monkeys(notes, monkeys)
        print *, 'solve monkey bounds: ', lbound(monkeys, 1), ubound(monkeys, 1)

        ! calcuate rounds of monkey business and show results
        do round = 1, 20
            call calculate_round(round, monkeys)
        end do

        ! return number of visited positions
        solve = calculate_monkeybusiness(monkeys)
    end function solve

end module day11a
