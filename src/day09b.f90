!> Solution for https://adventofcode.com/2021/day/9 part b
module day09b
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
            ! print *, direction, repeat
            totalmotions = totalmotions + repeat
        end do
        ! print *, totalmotions

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
    end subroutine

    subroutine execute_motions(motions, visitlist)
        implicit none

        type(ComplexList), intent(inout) :: motions
        type(ComplexList), intent(inout) :: visitlist
        integer                          :: i, link, lastlink
        complex                          :: motion, chainlinks(10)

        ! position all chainlinks at centre of coordinate system
        lastlink = size(chainlinks)
        do link = 1, lastlink
            chainlinks(link) = (0,0)
        end do

        ! execute all motions from the series
        do i = 1, motions%length()
            motion = motions%get(i)

            ! move head according to motion
            chainlinks(1) = chainlinks(1) + motion

            ! move all other chainlinks based on the drag of their forerunners
            do link = 2, lastlink
                chainlinks(link) = move_tail(chainlinks(link - 1), chainlinks(link))
            end do

            ! store new tail position only in visitlist if it is the very first visit
            if (visitlist%containsints(chainlinks(lastlink)) .eqv. .false.) then
                call visitlist%add(chainlinks(lastlink))
            end if

            ! call plot_playground(visitlist, chainlinks)
            ! print *
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
                ! drag most along x axis
                newtail = tail + cmplx(dragx / 2, dragy)
            else
                ! drag most along y axis
                newtail = tail + cmplx(dragx, dragy / 2)
            end if
        end if
    end function

    subroutine plot_playground(visitlist, chainlinks)
        implicit none

        type(ComplexList), intent(in)           :: visitlist
        complex, intent(in), optional           :: chainlinks(:)
        integer                                 :: minx, miny, maxx, maxy, v, row, column
        character(len=1), allocatable           :: fields(:,:)
        character(len=1), parameter             :: emptyfield = '.', visitedfield = '#', startfield = 's'
        complex                                 :: field

        ! find dimensions
        minx = -10
        miny = -10
        maxx = 10
        maxy = 10
        do v = 1, visitlist%length()
            minx = min(minx, int(real(visitlist%get(v))))
            miny = min(miny, int(aimag(visitlist%get(v))))
            maxx = max(maxx, int(real(visitlist%get(v))))
            maxy = max(maxy, int(aimag(visitlist%get(v))))
        end do
        if (present(chainlinks)) then
            do v = 1, size(chainlinks)
                minx = min(minx, int(real(chainlinks(v))))
                miny = min(miny, int(aimag(chainlinks(v))))
                maxx = max(maxx, int(real(chainlinks(v))))
                maxy = max(maxy, int(aimag(chainlinks(v))))
            end do
        end if
        ! print *, minx, miny, maxx, maxy

        ! prepare playground
        allocate(character(len=1) :: fields(miny:maxy,minx:maxx))
        ! print *, size(fields, 1), size(fields, 2)
        do row = miny, maxy
            do column = minx, maxx
                fields(row, column) = emptyfield
            end do
        end do

        ! mark all visited fields
        do v = 1, visitlist%length()
            field = visitlist%get(v)
            row = int(aimag(field))
            column = int(real(field))
            fields(row, column) = visitedfield
        end do

        ! makr startfield
        fields(0, 0) = startfield

        ! mark all occupied fields
        if (present(chainlinks)) then
            do v = size(chainlinks), 1, -1
                field = chainlinks(v)
                row = int(aimag(field))
                column = int(real(field))
                if (v == 1) then
                    fields(row, column) = 'H'
                else
                    fields(row, column) = char(ichar('1') + v - 2)
                end if
            end do
        end if

        ! print playground
        do row = miny, maxy
            print *, fields(row, :)
        end do
    end subroutine

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)   :: filename
        type(ComplexList) :: motions
        type(ComplexList) :: visitlist
        ! integer           :: i

        ! analyse series of motions and prepare containers
        call analyse_motions(filename, motions, visitlist)

        ! execute series of motions and drag tail around
        call execute_motions(motions, visitlist)
        ! do i = 1, visitlist%length()
        !     print *, visitlist%get(i)
        ! end do
        call plot_playground(visitlist)

        ! return number of visited positions
        solve = -1  ! visitlist%length()
        ! 2617 is wrong
    end function

end module day09b
