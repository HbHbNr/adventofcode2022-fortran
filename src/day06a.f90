!> Solution for https://adventofcode.com/2021/day/6 part a
module day06a
    use util, only : printioerror
    implicit none
    private

    public :: solve, unique_quartet

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

    integer function solve(filename)
        implicit none

        character(len=*), intent(in) :: filename
        integer                      :: io, iostat
        character(len=512)           :: iomsg
        character(len=4096)          :: line
        integer                      :: sopmarker  ! start-of-packet marker

        open(newunit=io, file=filename, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            call printioerror(iostat, iomsg, .true.)
        end if
        read(io, '(A)', iostat=iostat, iomsg=iomsg) line
        if (iostat /= 0) then
            ! end of file or I/O error -> exit loop
            call printioerror(iostat, iomsg, .true.)
        end if
        close(io)
        ! debug: output line from file and its length
        ! print '(A, A2, I4)', trim(line), ': ', len_trim(line)
        do sopmarker = 4, len(line)
            if (unique_quartet(line(sopmarker-3:sopmarker))) then
                exit
            end if
        end do
        ! return maximum calories
        solve = sopmarker
    end function

end module day06a
