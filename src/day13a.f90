!> Solution for https://adventofcode.com/2021/day/13 part a
module day13a
    use util, only : readinputfile_asstringarray
    implicit none
    private

    public :: solve

    type, public :: Token
        integer          :: int
        character(len=1) :: char
    end type Token

contains

    function extract_tokens(line) result(tokens)
        implicit none

        character(len=*)         :: line
        type(Token), allocatable :: tokens(:)
        integer                  :: i, j
        character(len=1)         :: char

        print *, line
        allocate(tokens(len(line)))
        i = 1
        j = 1
        do while (i <= len_trim(line))
            char = line(i:i)
            if (char == ',') then
                ! ignore comma
            else if (char == '[' .or. char == ']') then
                tokens(j)%int = -1
                tokens(j)%char = char
                print *, tokens(j)
                j = j + 1
            else
                tokens(j)%int = ichar(char) - ichar('1') + 1
                tokens(j)%char = ' '
                if (line(i+1:i+1) == '0') then
                    tokens(j)%int = 10
                    i = i + 1
                end if
                print *, tokens(j)
                j = j + 1
            end if
            i = i + 1
        end do
    end function

    function inrightorder(tokensleft, tokensright) result(ordered)
        implicit none

        type(Token) :: tokensleft(*), tokensright(*)
        logical     :: ordered

        ordered = .true.
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        type(Token), allocatable      :: tokensleft(:), tokensright(:)
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
    end function

end module day13a
