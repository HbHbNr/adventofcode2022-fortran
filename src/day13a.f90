!> Solution for https://adventofcode.com/2021/day/13 part a
module day13a
    use util, only : readinputfile_asstringarray
    use class_intringbuffer, only : IntRingBuffer
    implicit none
    private

    integer, parameter :: square_bracket_open = ichar('[')
    integer, parameter :: square_bracket_close = ichar(']')

    public :: solve

contains

    function extract_tokens(line) result(tokens)
        implicit none

        character(len=*)    :: line
        type(IntRingBuffer) :: tokens
        integer             :: i, int
        character(len=1)    :: char

        print *, line
        call tokens%init(len_trim(line) * 3)  ! enough space to surround every number with brackets
        i = 1
        do while (i <= len_trim(line))
            char = line(i:i)
            if (char == ',') then
                ! ignore comma
            else if (char == '[') then
                call tokens%addLast(square_bracket_open)
                print *, tokens%getLast()
            else if (char == ']') then
                call tokens%addLast(square_bracket_close)
                print *, tokens%getLast()
            else
                int = ichar(char) - ichar('1') + 1
                if (line(i+1:i+1) == '0') then
                    int = 10
                    i = i + 1
                end if
                call tokens%addLast(int)
                print *, tokens%getLast()
            end if
            i = i + 1
        end do
        call tokens%print()
    end function

    function inrightorder(tokensleft, tokensright) result(ordered)
        implicit none

        type(IntRingBuffer) :: tokensleft, tokensright
        logical             :: ordered

        ordered = .true.
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        type(IntRingBuffer)           :: tokensleft, tokensright
        integer                       :: pair, pairs, sumofindizes

        lines = readinputfile_asstringarray(filename, 200)

        sumofindizes = 0
        pairs = (size(lines) + 1) / 3
        do pair = 1, pairs
            tokensleft = extract_tokens(lines(pair * 3 - 2))
            tokensright = extract_tokens(lines(pair * 3 - 1))
                if (inrightorder(tokensleft, tokensright)) then
                sumofindizes = sumofindizes + pair
            end if
        end do

        ! return maximum calories
        solve = sumofindizes
        solve = -1
    end function

end module day13a
