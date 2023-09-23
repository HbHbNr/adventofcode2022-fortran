!> Collection of utility functions
module util
    implicit none
    private

    public :: printresultline_integer
    public :: printresultline
    public :: printioerror
    public :: readinputfile_asline
    public :: readinputfile_asarray

contains

    !> print a standard AOC result line, with an integer parameter
    subroutine printresultline_integer(day, intresult)
        implicit none

        character(len=*), intent(in) :: day
        integer(kind=4) , intent(in) :: intresult
        character(len=11)            :: result

        write(result, '(I11)') intresult
        call printresultline(day, result)
    end subroutine

    !> print a standard AOC result line, with a string parameter
    subroutine printresultline(day, result)
        implicit none

        character(len=*), intent(in) :: day
        character(len=*), intent(in) :: result

        print '(A, A, A, A)', 'Day ', day, ': ', adjustl(result)
    end subroutine

    !> print an I/O error message, and stop program on serious I/O error or by request
    subroutine printioerror(iostat, iomsg, alwaysstop)
        implicit none

        integer, intent(in)           :: iostat
        character(len=*), intent(in)  :: iomsg
        logical, intent(in), optional :: alwaysstop
        character(len=80)             :: iostatstr

        ! iostat:
        !   ==  0 -> no error
        !   == -1 -> end of file
        !    >  0 -> serious I/O error
        if (iostat > 0) then
            write (iostatstr, *) iostat
            print '(A, A, A, A, A)', 'I/O error: ', trim(adjustl(iostatstr)), ' (', trim(iomsg), ')'
            stop
        end if
        if (present(alwaysstop)) then
            if (alwaysstop .eqv. .true.) then
                stop
            end if
        end if
    end subroutine

    function readinputfile_asline(filename) result(line)
        implicit none

        character(len=*), intent(in)  :: filename
        integer                       :: io, iostat
        character(len=512)            :: iomsg
        integer                       :: filesize
        character(len=:), allocatable :: tmpline, line

        ! open file for reading
        open(newunit=io, file=filename, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            call printioerror(iostat, iomsg, .true.)
        end if

        ! get file size
        inquire(file=filename, size=filesize)

        ! allocate string to read complete file
        allocate(character(len=filesize) :: tmpline)
        read(io, '(A)', iostat=iostat, iomsg=iomsg) tmpline
        if (iostat /= 0) then
            ! end of file or I/O error -> exit loop
            call printioerror(iostat, iomsg, .true.)
        end if
        close(io)

        ! remove blanks or newlines at the end of the file
        line = tmpline(:len_trim(tmpline))
        deallocate(tmpline)
    end function

    function readinputfile_asarray(filename, linebufferlength) result(lines)
        implicit none

        character(len=*), intent(in)    :: filename
        integer, intent(in)             :: linebufferlength
        integer                         :: io, iostat
        character(len=512)              :: iomsg
        integer                         :: linecount, linelength, maxlinelength
        character(len=linebufferlength) :: tmpline
        character(len=linebufferlength) :: line
        character(len=linebufferlength), allocatable :: lines(:)

        ! open file for reading
        open(newunit=io, file=filename, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            call printioerror(iostat, iomsg, .true.)
        end if

        ! get file's number of lines 
        linecount = 0
        linelength = 0
        maxlinelength = 0
        do
            read(io, '(A)', iostat=iostat, iomsg=iomsg) tmpline
            if (iostat /= 0) then
                ! end of file or I/O error -> stop program
                call printioerror(iostat, iomsg)
                exit
            end if
            linecount = linecount + 1
            linelength = len_trim(tmpline)
            maxlinelength = max(maxlinelength, linelength)
        end do
        print *, 'linecount=', linecount
        print *, 'maxlinelength=', maxlinelength

        ! create new array and read all lines of file
        allocate(lines(linecount))
        rewind(io)
        linecount = 0
        do
            read(io, '(A)', iostat=iostat, iomsg=iomsg) tmpline
            if (iostat /= 0) then
                ! end of file or I/O error -> stop program
                call printioerror(iostat, iomsg)
                exit
            end if
            linecount = linecount + 1
            ! TODO: cut line to maxlinelength or actual length
            line = tmpline
            lines(linecount) = tmpline
            print *, linecount, line
        end do
        close(io)
    end function

end module util
