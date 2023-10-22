!> Solution for https://adventofcode.com/2021/day/1 part b
module day01b
    use util, only : readinputfile_asstringarray
    implicit none
    private

    public :: top_calories
    public :: solve

contains

    subroutine top_calories(newsum, top1, top2, top3)
        implicit none

        integer, intent(in) :: newsum
        integer, intent(inout) :: top1, top2, top3
        integer                :: tmp

        if (newsum > top3) then
            ! newsum should be within the top 3, so override the last value
            top3 = newsum
            ! check if the new value needs to bubble to the top
            if (top3 > top2) then
                tmp = top2
                top2 = top3
                top3 = tmp
                if (top2 > top1) then
                    tmp = top1
                    top1 = top2
                    top2 = tmp
                end if
            end if
        end if
        ! debug: show top 3 calory sums
        ! print *, top1, top2, top3
    end subroutine

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        character(len=5)              :: line
        integer                       :: i, linecalories, sumcalories = 0
        integer                       :: top1 = 0, top2 = 0, top3 = 0

        lines = readinputfile_asstringarray(filename, 5)

        do i = 1, size(lines)
            line = lines(i)
            ! debug: output line from file and its length
            ! print '(A, A2, I1)', trim(line), ': ', len_trim(line)
            if (len_trim(line) == 0) then
                ! reset sum for a new Elf
                if (sumcalories > 0) then
                    call top_calories(sumcalories, top1, top2, top3)
                end if
                sumcalories = 0
            else
                ! same Elf, so add calories to the sum
                read(line, *) linecalories
                sumcalories = sumcalories + linecalories
            end if
        end do
        ! check also the last Elf's data
        if (sumcalories > 0) then
            call top_calories(sumcalories, top1, top2, top3)
        end if

        ! return sum of top 3 calory sums
        solve = top1 + top2 + top3
    end function

end module day01b
