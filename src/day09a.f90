!> Solution for https://adventofcode.com/2021/day/9 part a
module day09a
    use iso_fortran_env, only : int8, error_unit
    use util, only : printioerror, readinputfile_asstringarray
    use class_complexlist, only : ComplexList
    implicit none
    private

    public :: solve

contains

    subroutine analyse_motions(filename, motions, visitlist)
        implicit none

        character(len=*), intent(in)   :: filename
        type(ComplexList), intent(out) :: motions
        type(ComplexList), intent(out) :: visitlist
        character(len=:), allocatable  :: lines(:)
        character(len=5)               :: line
        character(len=1)               :: direction
        integer                        :: totalmotions, repeat, i, r
        complex                        :: motion

        ! read all motions into an array of strings
        lines = readinputfile_asstringarray(filename, len(line))

        ! iterate lines and count total number of motions
        totalmotions = 0
        do i=1, size(lines)
            read(lines(i), *) direction, repeat
            totalmotions = totalmotions + repeat
        end do

        ! prepare containers
        call motions%init(totalmotions)
        call visitlist%init(totalmotions)

        ! iterate lines again and create motions
        do i=1, size(lines)
            read(lines(i), *) direction, repeat
            select case (direction)
            case ('U')
                motion = (0, -1)
            case ('D')
                motion = (0, 1)
            case ('L')
                motion = (-1, 0)
            case ('R')
                motion = (1, 0)
            end select
            do r=1, repeat
                call motions%add(motion)
            end do
        end do
    end subroutine analyse_motions

    subroutine execute_motions(motions, visitlist)
        implicit none

        type(ComplexList), intent(inout) :: motions
        type(ComplexList), intent(inout) :: visitlist
        integer                          :: i
        complex                          :: motion, head, tail

        ! position head and tail at centre of coordinate system
        head = (0,0)
        tail = (0,0)

        ! execute all motions from the series
        do i = 1, motions%length()
            motion = motions%get(i)

            ! move head according to motion
            head = head + motion

            ! move tail based on the drag of head
            tail = move_tail(head, tail)

            ! store new tail position only in visitlist if it is the very first visit
            if (visitlist%containsints(tail) .eqv. .false.) then
                call visitlist%add(tail)
            end if
        end do
    end subroutine execute_motions

    function move_tail(head, tail) result(newtail)
        complex, intent(in) :: head, tail
        complex             :: newtail, drag
        integer             :: dragx, dragy

        ! default: tail stays at the same place
        newtail = tail

        ! calculate the drag from tail towards head
        drag = head - tail
        dragx = int(real(drag))
        dragy = int(aimag(drag))

        ! analyse drag and move tail only if needed
        if (abs(dragx) > 1 .or. abs(dragy) > 1) then
            if (dragx == 0 .or. dragy == 0) then
                ! drag straight along x or y axis
                newtail = tail + (drag / 2)
            else if (abs(dragx) > abs(dragy)) then
                ! drag mostly along x axis
                newtail = tail + cmplx(dragx / 2, dragy)
            else
                ! drag mostly along y axis
                newtail = tail + cmplx(dragx, dragy / 2)
            end if
        end if
    end function move_tail

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)   :: filename
        type(ComplexList) :: motions
        type(ComplexList) :: visitlist

        ! analyse series of motions and prepare containers
        call analyse_motions(filename, motions, visitlist)

        ! execute series of motions and drag tail around
        call execute_motions(motions, visitlist)

        ! return number of visited positions
        solve = visitlist%length()
    end function solve

end module day09a
