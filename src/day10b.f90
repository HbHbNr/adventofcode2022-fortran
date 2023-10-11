!> Solution for https://adventofcode.com/2021/day/10 part b
module day10b
    use util, only : readinputfile_asstringarray
    use class_intstack, only : IntStack
    implicit none
    private

    public :: solve

contains

    function execute_instructions(instructions) result(patterns)
        implicit none

        character(len=*), intent(in)  :: instructions(:)
        character(len=40)             :: patterns(6)
        character(len=:), allocatable :: instruction
        character(len=4)              :: command
        integer                       :: cycle_, positionx, positiony, registerX, value, i
        type(IntStack)                :: stack
        character(len=1)              :: dot, setdot = '#', nodot = ' '

        ! prepare variables
        cycle_ = 1
        registerX = 1
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
                ! calculate X and Y position and set a dot or not a dot
                positionx = modulo(cycle_ - 1, 40)
                positiony = (cycle_ - 1) / 40
                if (abs(positionx - registerX) <= 1) then
                    dot = setdot
                else
                    dot = nodot
                end if
                write(patterns(positiony+1)(positionx+1:positionx+1),'(A1)') dot

                ! after the cycle: modify register and increase cycle number
                value = stack%pop()
                registerX = registerX + value
                cycle_ = cycle_ + 1  ! cycle number of the next cycle
            end do

            ! pattern = -1
        end do

    end function execute_instructions

    function solve(filename) result(patterns)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: instructions(:)
        character(len=40)             :: patterns(6)

        ! read list of instructions from file
        instructions = readinputfile_asstringarray(filename, 8)

        ! execute list of instructions and calculate sum of signal strengths
        patterns = execute_instructions(instructions)

        ! return number of visited positions
    end function solve

end module day10b
