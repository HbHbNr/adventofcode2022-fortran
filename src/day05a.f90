!> Solution for https://adventofcode.com/2021/day/5 part a
module day05a
    use class_CharStack, only : CharStack
    use util, only : printioerror
    implicit none
    private

    ! make public for unit testing
    public :: solve, init_stacks, print_stacks

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
            ! read until empty line, marking init block is over
            if (len_trim(line) == 0) then
                linecount = linecount - 2

                ! get last char of the line and convert it to integer
                lastchar = lastline(len_trim(lastline):len_trim(lastline))
                read(lastchar, '(I1)') stackcount
                exit
            end if
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

    subroutine perform_steps(io, stacks)
        implicit none

        type(CharStack), intent(inout) :: stacks(:)
        integer                        :: io, iostat
        character(len=512)             :: iomsg
        character(len=64)              :: line, move*4, from*4, to*2, char*1
        integer                        :: amount, fromstack, tostack
        integer                        :: skiplines

        skiplines = 2
        do
            read(io, '(A)', iostat=iostat, iomsg=iomsg) line
            if (iostat /= 0) then
                ! end of file or I/O error -> exit loop
                call printioerror(iostat, iomsg)
                exit
            end if
            ! skip the stack number line and the empty line
            if (skiplines > 0) then
                skiplines = skiplines - 1
                cycle
            end if
            ! debug: output line from file and its length
            ! print '(A22, A2, I2)', line, ': ', len_trim(line)

            ! split line into step details
            read(line, *) move, amount, from, fromstack, to, tostack
            ! execute step details
            do while (amount > 0)
                char = stacks(fromstack)%pop()
                call stacks(tostack)%push(char)
                amount = amount - 1
            end do
        end do
    end subroutine

    function peek_stacks(stacks) result(peek)
        implicit none

        type(CharStack), intent(in)   :: stacks(:)
        character(len=:), allocatable :: peek
        integer                       :: i, j

        allocate(character(len=size(stacks)) :: peek)
        ! gather top data only from stacks which are not empty
        j = 1
        do i = 1, size(stacks)
            if (stacks(i)%size() > 0) then
                peek(j:j) = stacks(i)%peek()
                j = j + 1
            end if
        end do
    end function

    subroutine print_stacks(stacks)
        implicit none

        type(CharStack), intent(in)   :: stacks(:)
        integer                       :: i

        do i = 1, size(stacks)
            call stacks(i)%print()
        end do
        print *, '----------'
    end subroutine

    function solve(filename) result(result)
        implicit none

        character(len=*), intent(in)   :: filename
        integer                        :: io, iostat
        character(len=512)             :: iomsg
        integer                        :: linecount, stackcount
        character(len=64), allocatable :: lines(:)
        type(CharStack), allocatable   :: stacks(:)
        character(len=:), allocatable  :: result

        open(newunit=io, file=filename, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            call printioerror(iostat, iomsg, .true.)
        end if

        ! analyse the initial block get its size and the count of stacks
        call analyse_init_block(io, linecount, stackcount)

        ! start from beginning of file again and read initial block
        rewind(io)
        lines = read_init_block(io, linecount)

        ! allocate and initialise stacks with the lines from the initial block
        allocate(stacks(stackcount), source=CharStack('', 0))
        call init_stacks(lines, stacks)

        ! execute the steps of the rearrangement procedure
        call perform_steps(io, stacks)
        close(io)

        ! concatenate the top elements of each stack
        result = peek_stacks(stacks)
    end function

end module day05a
