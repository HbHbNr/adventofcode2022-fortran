!> Solution for https://adventofcode.com/2021/day/6 part a
module day06a
    use util, only : printioerror, readinputfile_asline
    implicit none
    private

    public :: solve, unique_quartet, find_sopmarker

contains

    logical function unique_quartet(quartet)
        implicit none

        character(len=4), intent(in) :: quartet
        integer                      :: i, j

        unique_quartet = .true.
        iloop: do i = 1, 3
            do j = i+1, 4
                if (quartet(i:i) == quartet(j:j)) then
                    unique_quartet = .false.
                    exit iloop
                end if
            end do
        end do iloop
    end function

    integer function find_sopmarker(line)
        implicit none

        character(len=*), intent(in) :: line
        integer                      :: sopmarker  ! start-of-packet marker

        do sopmarker = 4, len(line)
            if (unique_quartet(line(sopmarker-3:sopmarker))) then
                exit
            end if
        end do
        find_sopmarker = sopmarker
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: line

        line = readinputfile_asline(filename)
        ! debug: output line from file and its length
        ! print '(A, A2, I4)', trim(line), ': ', len_trim(line)
        solve = find_sopmarker(line)
    end function

end module day06a
