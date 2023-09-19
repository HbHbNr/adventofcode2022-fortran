!> Solution for https://adventofcode.com/2021/day/5 part a
module day05a
    use class_CharStack, only : CharStack
    use util, only : printioerror
    implicit none
    private

    public :: solve

contains

    subroutine analyse_init_block(io, linecount, stackcount)
        implicit none

        integer              :: io, iostat
        character(len=512)   :: iomsg
        integer, intent(out) :: linecount, stackcount
        character(len=64)    :: line, lastline
        character(len=1)     :: lastchar

        linecount = 0
        stackcount = 0
        line = ''
        lastline = ''
        do
            linecount = linecount + 1
            lastline = line
            read(io, '(A)', iostat=iostat, iomsg=iomsg) line
            if (iostat /= 0) then
                ! end of file or I/O error -> stop program
                call printioerror(iostat, iomsg, .true.)
                exit
            end if
            if (len_trim(line) == 0) then
                ! empty line: init block over
                linecount = linecount - 2
                ! print *, lastline
                lastchar = lastline(len_trim(lastline):len_trim(lastline))
                ! print *, 'lastchar=', lastchar
                read(lastchar, '(I1)') stackcount
                exit
            end if
            ! print *, lines
        end do
    end subroutine

    function read_init_block(io, linecount) result(lines)
        implicit none

        integer                        :: io, iostat
        character(len=512)             :: iomsg
        integer, intent(in)            :: linecount
        integer                        :: lineno
        character(len=64), allocatable :: lines(:)
        character(len=64)              :: line

        allocate(lines(linecount))
        ! read the lines of the init block into an array
        do lineno = 1, linecount
            read(io, '(A)', iostat=iostat, iomsg=iomsg) line
            if (iostat /= 0) then
                ! end of file or I/O error -> stop program
                call printioerror(iostat, iomsg, .true.)
                exit
            end if
            lines(lineno) = line
        end do
    end function

    subroutine init_stacks(lines, stacks)
        implicit none

        character(len=64), intent(in)  :: lines(:)
        type(CharStack), intent(inout) :: stacks(:)
        integer                        :: lineno, stackno, charpos
        character(len=64)              :: line
        character                      :: char

        ! iterate through all lines in reverse order and fill the stacks accordingly
        do lineno = size(lines), 1, -1
            line = lines(lineno)
            do stackno = 1, size(stacks)
                charpos = (stackno * 4) - 2
                char = line(charpos:charpos)
                if (char /= ' ') then
                    call stacks(stackno)%push(char)
                end if
            end do
        end do
    end subroutine

    character(len=9) function solve(filename)
        implicit none

        character(len=*), intent(in)   :: filename
        integer                        :: io, iostat
        character(len=512)             :: iomsg
        integer                        :: linecount, stackcount
        character(len=64), allocatable :: lines(:)
        type(CharStack), allocatable   :: stacks(:)
        ! character(len=5)             :: line
        ! integer                      :: linecalories, sumcalories = 0, maxcalories = 0

        open(newunit=io, file=filename, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            call printioerror(iostat, iomsg, .true.)
        end if
        call analyse_init_block(io, linecount, stackcount)
        print *, linecount, stackcount
        allocate(stacks(stackcount), source=CharStack('', 0))
        rewind(io)
        lines = read_init_block(io, linecount)
        print *, lines
        call init_stacks(lines, stacks)
        print *, stacks
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
        close(io)
        ! return maximum calories
        solve = '_________'
    end function

end module day05a
