!> Solution for https://adventofcode.com/2022/day/13 part a
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

        call tokens%init(len_trim(line) * 3)  ! enough space to surround every number with brackets
        i = 1
        do while (i <= len_trim(line))
            char = line(i:i)
            if (char == ',') then
                ! ignore comma
            else if (char == '[') then
                call tokens%addLast(square_bracket_open)
            else if (char == ']') then
                call tokens%addLast(square_bracket_close)
            else
                int = ichar(char) - ichar('1') + 1
                if (line(i+1:i+1) == '0') then
                    int = 10
                    i = i + 1
                end if
                call tokens%addLast(int)
            end if
            i = i + 1
        end do
    end function

    function inrightorder(tokensleft, tokensright) result(ordered)
        implicit none

        type(IntRingBuffer) :: tokensleft, tokensright
        logical             :: ordered, scanning
        integer             :: left, right

        ordered = .false.
        scanning = .true.
        do while (scanning)
            left = tokensleft%removeFirst()
            right = tokensright%removeFirst()
            if (left < 11 .and. right < 11) then
                ! both tokens are numbers
                if (left < right) then
                    ordered = .true.
                    scanning = .false.
                else if (left > right) then
                    ordered = .false.
                    scanning = .false.
                else
                    ! both nunbers are equal: next iteration
                end if
            else if (left == square_bracket_open .and. right == square_bracket_open) then
                ! both lists start at the same time: next iteration
            else if (left == square_bracket_close .and. right == square_bracket_close) then
                ! both lists run out of items at the same time: next iteration
            else if (left == square_bracket_close .and. right < 11) then
                ! left list runs out of items first: ordered
                ordered = .true.
                scanning = .false.
            else if (right == square_bracket_close .and. left < 11) then
                ! right list runs out of items first: not ordered
                ordered = .false.
                scanning = .false.
            else if (left == square_bracket_close .and. right == square_bracket_open) then
                ! left list runs out of items first: ordered
                ordered = .true.
                scanning = .false.
            else if (right == square_bracket_close .and. left == square_bracket_open) then
                ! right list runs out of items first: not ordered
                ordered = .false.
                scanning = .false.
            else if (left == square_bracket_open .and. right < 11) then
                ! only right token is a number: make right a list with that one number,
                ! and re-add tokens
                call tokensleft%addFirst(left)
                call tokensright%addFirst(square_bracket_close)
                call tokensright%addFirst(right)
                call tokensright%addFirst(square_bracket_open)
            else if (right == square_bracket_open .and. left < 11) then
                ! only left token is a number: make left a list with that one number,
                ! and re-add tokens
                call tokensright%addFirst(right)
                call tokensleft%addFirst(square_bracket_close)
                call tokensleft%addFirst(left)
                call tokensleft%addFirst(square_bracket_open)
            else
                print *, 'unknown case:', left, right
                stop
            end if
        end do
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

        solve = sumofindizes
    end function

end module day13a
