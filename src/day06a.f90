!> Solution for https://adventofcode.com/2021/day/6 part a
module day06a
    use util, only : printioerror, readinputfile_asline
    implicit none
    private

    public :: solve, unique_string, find_sopmarker

contains

    ! check if a string consist of only unique characters
    logical function unique_string(string)
        implicit none

        character(len=*), intent(in) :: string
        integer                      :: i, j

        unique_string = .true.
        ! print *, string
        iloop: do i = 1, len(string)-1
            do j = i+1, len(string)
                if (string(i:i) == string(j:j)) then
                    unique_string = .false.
                    exit iloop
                end if
            end do
        end do iloop
    end function

    ! find start-of-packet marker in a string
    integer function find_sopmarker(line)
        implicit none

        character(len=*), intent(in) :: line
        integer                      :: sopmarker  ! start-of-packet marker
        integer, parameter           :: sopmarkerlength = 4

        do sopmarker = sopmarkerlength, len(line)
            if (unique_string(line(sopmarker-sopmarkerlength+1:sopmarker))) then
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
