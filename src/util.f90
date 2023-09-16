!> Collection of utility functions
module util
    implicit none
    private

    public :: printresultline_integer
    public :: printresultline
    public :: printioerror

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

end module util
