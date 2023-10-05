!> Solution for https://adventofcode.com/2021/day/10 part a
module day10a
    use util, only : printioerror, readinputfile_asstringarray
    implicit none
    private

    public :: solve

contains

    function execute_instructions(instructions) result(signalsum)
        implicit none

        character(len=*), intent(in)  :: instructions(:)
        character(len=:), allocatable :: instruction
        character(len=4)              :: command
        integer                       :: signalsum, cycle_, registerX, value, i

        cycle_ = 0
        registerX = 1
        signalsum = 0
        allocate(character(len=len(instructions(1))) :: instruction)
        do i = 1, size(instructions)
            instruction = instructions(i)
            if (trim(instruction) == 'noop') then
                cycle_ = cycle_ + 1
                print *, 'noop'
            else
                read(instruction, *) command, value
                cycle_ = cycle_ + 2
                registerX = registerX + value
                print *, command, value
            end if
            print *, registerX
            signalsum = signalsum + registerX
            print *, signalsum
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
        print *, signalsum

        ! return number of visited positions
        solve = -1  ! signalsum
    end function solve

end module day10a
