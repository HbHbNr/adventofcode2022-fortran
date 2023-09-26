!> Solution for https://adventofcode.com/2021/day/8 part a
module day08a
    use util, only : printioerror
    implicit none
    private

    public :: solve

contains

    integer function solve(filename)
        implicit none

        character(len=*), intent(in) :: filename
        integer                      :: io, iostat
        character(len=512)           :: iomsg
        character(len=5)             :: line
        integer                      :: linecalories, sumcalories = 0, maxcalories = 0

        open(newunit=io, file=filename, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            call printioerror(iostat, iomsg, .true.)
        end if
        do
            read(io, '(A)', iostat=iostat, iomsg=iomsg) line
            if (iostat /= 0) then
                ! end of file or I/O error -> exit loop
                call printioerror(iostat, iomsg)
                exit
            end if
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
        close(io)
        ! return maximum calories
        solve = maxcalories
    end function

end module day08a
