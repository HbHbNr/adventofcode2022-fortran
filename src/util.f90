!> Collection of utility functions
module util
    implicit none
    private

    public :: printresultline_integer
    public :: printresultline
    public :: printioerror
    public :: readinputfile_asline
    public :: readinputfile_asstringarray
    public :: readinputfile_asintarray

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

    !> read the first (and maybe only) line of a file into a string
    function readinputfile_asline(filename) result(line)
        implicit none

        character(len=*), intent(in)  :: filename
            !! name of the file
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

    !> read all lines of a file into an array of strings, with the length
    !> of the strings fitting to the longest line
    function readinputfile_asstringarray(filename, linebufferlength) result(lines)
        implicit none

        character(len=*), intent(in)    :: filename
            !! name of the file
        integer, intent(in)             :: linebufferlength
            !! length of a buffer for one line, big enough for the longest line
        integer                         :: io, iostat
        character(len=512)              :: iomsg
        integer                         :: linecount, linelength, maxlinelength
        character(len=linebufferlength) :: tmpline
        character(len=:), allocatable   :: line
        character(len=:), allocatable   :: lines(:)

        ! open file for reading
        open(newunit=io, file=filename, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            call printioerror(iostat, iomsg, .true.)
        end if

        ! get file's number of lines and length of the longest line
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
        ! print *, 'linecount=', linecount
        ! print *, 'maxlinelength=', maxlinelength

        ! create new array and read all lines from file
        allocate(character(len=maxlinelength) :: lines(linecount))
        allocate(character(len=maxlinelength) :: line)
        rewind(io)
        linecount = 0
        do
            read(io, '(A)', iostat=iostat, iomsg=iomsg) line
            if (iostat /= 0) then
                ! end of file or I/O error -> stop program
                call printioerror(iostat, iomsg)
                exit
            end if
            linecount = linecount + 1
            lines(linecount) = line
            ! print *, '"', line, '"', linecount
        end do
        close(io)
    end function

    !> read all lines of a file into an array of int, with the dimensions
    !> of the array fitting to the content of the rectangular file
    function readinputfile_asintarray(filename, linebufferlength) result(intarray)
        use iso_fortran_env, only : int8
        implicit none

        character(len=*), intent(in)    :: filename
            !! name of the file
        integer, intent(in)             :: linebufferlength
            !! length of a buffer for one line, big enough for the longest line
        integer                         :: io, iostat
        character(len=512)              :: iomsg
        integer                         :: linecount, linelength
        character(len=linebufferlength) :: tmpline
        character(len=20)               :: format
        integer(int8), allocatable      :: tmpintarray(:,:), intarray(:,:)

        ! open file for reading
        open(newunit=io, file=filename, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            call printioerror(iostat, iomsg, .true.)
        end if

        ! get file's number of lines and length per line
        linecount = 0
        linelength = 0
        do
            read(io, '(A)', iostat=iostat, iomsg=iomsg) tmpline
            if (iostat /= 0) then
                ! end of file or I/O error -> stop program
                call printioerror(iostat, iomsg)
                exit
            end if
            linecount = linecount + 1
            linelength = len_trim(tmpline)
        end do
        ! print *, 'linecount=', linecount
        ! print *, 'linelength=', linelength

        ! create new array and read all lines from file
        allocate(tmpintarray(linelength,linecount))
        ! allocate(intarray(linecount, linelength))
        rewind(io)
        write(format, *) '(', linelength, '(I1))'  ! format for a single line of the array
        read(io, format, iostat=iostat, iomsg=iomsg) tmpintarray
        if (iostat /= 0) then
            ! end of file or I/O error -> stop program
            call printioerror(iostat, iomsg, .true.)
        end if
        intarray = transpose(tmpintarray)
        deallocate(tmpintarray)
        close(io)
    end function

end module util
