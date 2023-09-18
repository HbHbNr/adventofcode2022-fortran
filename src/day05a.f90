!> Solution for https://adventofcode.com/2021/day/5 part a
module day05a
    use util, only : printioerror
    implicit none
    private

    public :: solve

contains

    subroutine analyse_init_block(io, lines, stackcount)
        implicit none

        integer              :: io, iostat
        character(len=512)   :: iomsg
        integer, intent(out) :: lines, stackcount
        character(len=64)    :: line, lastline
        character(len=1)     :: lastchar

        lines = 0
        stackcount = 0
        line = ''
        lastline = ''
        do
            lines = lines + 1
            lastline = line
            read(io, '(A)', iostat=iostat, iomsg=iomsg) line
            if (iostat /= 0) then
                ! end of file or I/O error -> stop program
                call printioerror(iostat, iomsg, .true.)
                exit
            end if
            if (len_trim(line) == 0) then
                ! empty line: init block over
                lines = lines - 2
                ! print *, lastline
                lastchar = lastline(len_trim(lastline):len_trim(lastline))
                ! print *, 'lastchar=', lastchar
                read(lastchar, '(I1)') stackcount
                exit
            end if
            ! print *, lines
        end do
    end subroutine

    character(len=9) function solve(filename)
        implicit none

        character(len=*), intent(in) :: filename
        integer                      :: io, iostat
        character(len=512)           :: iomsg
        integer                      :: lines, stackcount
        ! character(len=5)             :: line
        ! integer                      :: linecalories, sumcalories = 0, maxcalories = 0

        open(newunit=io, file=filename, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            call printioerror(iostat, iomsg, .true.)
        end if
        do
            call analyse_init_block(io, lines, stackcount)
            print *, lines, stackcount
            rewind(io)
            exit
            ! read(io, '(A)', iostat=iostat, iomsg=iomsg) line
            ! if (iostat /= 0) then
            !     ! end of file or I/O error -> exit loop
            !     call printioerror(iostat, iomsg)
            !     exit
            ! end if
            ! ! debug: output line from file and its length
            ! ! print '(A5, A2, I1)', line, ': ', len_trim(line)
            ! if (len_trim(line) == 0) then
            !     ! reset sum for a new Elf
            !     sumcalories = 0
            ! else
            !     ! same Elf, so add calories to the sum
            !     read(line, *) linecalories
            !     sumcalories = sumcalories + linecalories
            !     maxcalories = max(maxcalories, sumcalories)
            ! end if
        end do
        close(io)
        ! return maximum calories
        solve = '_________'
    end function

end module day05a
