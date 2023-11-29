!> Solution for https://adventofcode.com/2022/day/1 part a
module day01a
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 5

    public :: solve

contains

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        character(len=maxlinelength)  :: line
        integer                       :: i, linecalories, sumcalories = 0, maxcalories = 0

        lines = readinputfile_asstringarray(filename, maxlinelength)

        do i = 1, size(lines)
            line = lines(i)
            ! debug: output line from file and its length
            ! print '(A, A2, I1)', trim(line), ': ', len_trim(line)
            if (len_trim(line) == 0) then
                ! reset sum for a new Elf
                sumcalories = 0
            else
                ! same Elf, so add calories to the sum
                read(line, *) linecalories
                sumcalories = sumcalories + linecalories
                maxcalories = max(maxcalories, sumcalories)
            end if
        end do

        ! return maximum calories
        solve = maxcalories
    end function

end module day01a
