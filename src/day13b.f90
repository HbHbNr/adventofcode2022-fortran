!> Solution for https://adventofcode.com/2021/day/13 part b
module day13b
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

    function inrightorder(tokensleftorg, tokensrightorg) result(ordered)
        implicit none

        type(IntRingBuffer), intent(in) :: tokensleftorg, tokensrightorg
        type(IntRingBuffer)             :: tokensleft, tokensright
        logical                         :: ordered, scanning
        integer                         :: left, right

        tokensleft = tokensleftorg
        tokensright = tokensrightorg
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

    recursive subroutine merge_sort(tokenslines, from, to, tokenstmp)
        implicit none

        type(IntRingBuffer), intent(inout) :: tokenslines(:), tokenstmp(:)
        integer, intent(in)                :: from, to
        integer                            :: from2, index1, index2, indexall

        ! only sort if more than one element
        if (from < to) then
            ! divide and sort both parts
            from2 = (from + to + 1) / 2
            call merge_sort(tokenslines, from, from2 - 1, tokenstmp)
            call merge_sort(tokenslines, from2, to, tokenstmp)

            ! merge both parts into tmp buffer
            index1 = from
            index2 = from2
            indexall = from
            do while(index1 <= from2 - 1 .and. index2 <= to)
                if (inrightorder(tokenslines(index1), tokenslines(index2))) then
                    tokenstmp(indexall) = tokenslines(index1)
                    index1 = index1 + 1
                else
                    tokenstmp(indexall) = tokenslines(index2)
                    index2 = index2 + 1
                end if
                indexall = indexall + 1
            end do
            do while(index1 <= from2 - 1)
                tokenstmp(indexall) = tokenslines(index1)
                index1 = index1 + 1
                indexall = indexall + 1
            end do
            do while(index2 <= to)
                tokenstmp(indexall) = tokenslines(index2)
                index2 = index2 + 1
                indexall = indexall + 1
            end do

            ! copy tmp buffer back to intial array
            tokenslines(from:to) = tokenstmp(from:to)
        end if
    end subroutine

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)     :: filename
        character(len=:), allocatable    :: lines(:)
        type(IntRingBuffer)              :: tokensline, devider2, devider6
        type(IntRingBuffer), allocatable :: tokenslines(:), tokenstmp(:)
        integer                          :: pair, pairs, i, decoderkey

        lines = readinputfile_asstringarray(filename, 200)

        ! prepare list of lists of tokens
        pairs = (size(lines) + 1) / 3
        allocate(tokenslines(pairs * 2 + 2))
        allocate(tokenstmp(size(tokenslines)))
        devider2 = extract_tokens('[[2]]')
        tokenslines(1) = devider2
        devider6 = extract_tokens('[[6]]')
        tokenslines(2) = devider6
        do pair = 1, pairs
            tokensline = extract_tokens(lines(pair * 3 - 2))
            tokenslines(pair * 2 + 1) = tokensline
            tokensline = extract_tokens(lines(pair * 3 - 1))
            tokenslines(pair * 2 + 2) = tokensline
        end do

        ! sort list of lists of tokens
        call merge_sort(tokenslines, 1, size(tokenslines), tokenstmp)

        ! find devider packets
        decoderkey = 1
        do i = 1, size(tokenslines)
            tokensline = tokenslines(i)
            if (tokensline%length() == 5) then
                if (tokensline%get(3) == 2 .or. tokensline%get(3) == 6) then
                    if (tokensline%get(2) == square_bracket_open .and. &
                        tokensline%get(4) == square_bracket_close) then
                        decoderkey = decoderkey * i
                    end if
                end if
            end if
        end do

        ! TODO: search for deviders

        solve = decoderkey
    end function

end module day13b
