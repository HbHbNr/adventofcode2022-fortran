!> Solution for https://adventofcode.com/2021/day/7 part a
module day07a
    use util, only : printioerror, readinputfile_asarray
    implicit none
    private

    public :: solve

contains

    ! recursively traverse directory structure and sum up all file sizes
    recursive integer function calcdirsize(lines, currentstep, currentdir) result(thisdirsize)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer, intent(inout)        :: currentstep
        character(len=*), intent(in)  :: currentdir
        character(len=:), allocatable :: line, filename
        integer                       :: filesize

        allocate(character(len=len(lines)) :: line)
        allocate(character(len=len(lines)) :: filename)
        thisdirsize = 0

        ! check "cd" line
        line = lines(currentstep)
        if (line(1:4) /= '$ cd') then
            print *, 'expected "$ cd", but found ', line
            stop
        else
            print *, 'leaving ', currentdir, ', entering ', trim(line(6:))
        end if

        ! check all lines until the end or until "cd .." command
        do
            currentstep = currentstep + 1
            ! already at the end?
            if (currentstep > size(lines)) then
                print *, 'collected ', thisdirsize, ' bytes'
                return
            end if

            ! examine current line
            line = lines(currentstep)
            print *, currentdir, '> ', line
            if (line(1:4) == '$ ls') then
                ! ignore '$ ls'
                cycle
            else if (line(1:4) == 'dir ') then
                ! ignore 'dir '
                cycle
            else if (line(1:7) == '$ cd ..') then
                ! return from recursion on '$ cd ..'
                print *, 'collected ', thisdirsize, ' bytes'
                return
            else if (line(1:4) == '$ cd') then
                ! deeper recursion on other '$ cd'
                thisdirsize = thisdirsize + calcdirsize(lines, currentstep, currentdir)
            else
                ! if nothing else fit, the line must be a file and its size
                read(line, *) filesize, filename
                print *, 'file ', filename, ' has size ', filesize
                thisdirsize = thisdirsize + filesize
            end if
        end do
    end function calcdirsize

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)    :: filename
        integer, parameter              :: linebufferlength = 32
        character(len=:), allocatable   :: lines(:)
        character(len=:), allocatable   :: line
        integer                         :: i, totaldirsize, currentstep

        ! read whole file into an array of strings
        print *, 'open file ', filename, ' for reading'
        lines = readinputfile_asarray(filename, linebufferlength)
        do i = 1, size(lines)
            line = lines(i)
            print *, '"', line, '"'
        end do

        ! traverse lines and calculate the total directory size
        currentstep = 1
        totaldirsize = calcdirsize(lines, currentstep, '')

        solve = totaldirsize
    end function

end module day07a
