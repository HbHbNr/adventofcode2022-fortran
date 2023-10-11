!> Solution for https://adventofcode.com/2021/day/10 part a
module day10a
    use util, only : readinputfile_asstringarray
    use class_intstack, only : IntStack
    implicit none
    private

    public :: solve

contains

    function execute_instructions(instructions) result(signalsum)
        implicit none

        character(len=*), intent(in)  :: instructions(:)
        character(len=:), allocatable :: instruction
        character(len=4)              :: command
        integer                       :: signalstrength, signalsum, cycle_, registerX, value, i
        type(IntStack)                :: stack
        integer, parameter            :: triggercycles(6) = [20, 60, 100, 140, 180, 220]

        ! prepare variables
        cycle_ = 1
        registerX = 1
        signalsum = 0
        call stack%init(3)
        allocate(character(len=len(instructions(1))) :: instruction)

        ! iterate through all instructions
        do i = 1, size(instructions)
            instruction = instructions(i)
            if (trim(instruction) == 'noop') then
                call stack%push(0)      ! only step: add nothing
            else
                read(instruction, *) command, value
                call stack%push(value)  ! 2. step: add value
                call stack%push(0)      ! 1. step: add nothing
            end if

            ! execute commands on the stack
            do while (stack%size() > 0)
                ! check if this beginning cycle is one of the trigger cycles; the value at the beginning stays the
                ! same during the cycle; only *after* the cycle the register is modified
                if (any(triggercycles == cycle_)) then
                    signalstrength = cycle_ * registerX
                    signalsum = signalsum + signalstrength
                end if

                ! after the cycle: modify register and increase cycle number
                value = stack%pop()
                registerX = registerX + value
                cycle_ = cycle_ + 1  ! cycle number of the next cycle
            end do
        end do

    end function execute_instructions

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: instructions(:)
        integer                       :: signalsum

        ! read list of instructions from file
        instructions = readinputfile_asstringarray(filename, 8)

        ! execute list of instructions and calculate sum of signal strengths
        signalsum = execute_instructions(instructions)

        ! return number of visited positions
        solve = signalsum
    end function solve

end module day10a
