!> Solution for https://adventofcode.com/2021/day/7 part b
module day07b
    use util, only : printioerror, readinputfile_asstringarray
    implicit none
    private

    public :: solve

contains

    ! recursively traverse directory structure and sum up all file sizes;
    ! missingdiskspace and bestdeletedir will be set at a second run to find the best directory to be deleted
    recursive integer function calcdirsize(lines, currentstep, missingdiskspace, bestdeletedir) result(thisdirsize)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer, intent(inout)        :: currentstep
        integer, intent(in)           :: missingdiskspace
        integer, intent(inout)        :: bestdeletedir
        character(len=:), allocatable :: line, currentdir, filename
        integer                       :: filesize

        allocate(character(len=len(lines)) :: line)
        allocate(character(len=len(lines)) :: currentdir)
        allocate(character(len=len(lines)) :: filename)
        thisdirsize = 0

        ! check "cd" line
        line = lines(currentstep)
        if (line(1:4) /= '$ cd') then
            print *, 'expected "$ cd", but found ', line
            stop
        else
            currentdir = trim(line(6:))
            ! print *, 'entering ', currentdir
        end if

        ! check all lines until the end or until "cd .." command
        do
            currentstep = currentstep + 1
            ! already at the end?
            if (currentstep > size(lines)) then
                ! print *, 'collected ', thisdirsize, ' bytes in directory ', currentdir
                if ((thisdirsize >= missingdiskspace) .and. (thisdirsize < bestdeletedir)) then
                    bestdeletedir = thisdirsize
                end if
                return
            end if

            ! examine current line
            line = lines(currentstep)
            ! print *, currentdir, '> ', line
            if (line(1:4) == '$ ls') then
                ! ignore '$ ls'
                cycle
            else if (line(1:4) == 'dir ') then
                ! ignore 'dir '
                cycle
            else if (line(1:7) == '$ cd ..') then
                ! return from recursion on '$ cd ..'
                ! print *, 'collected ', thisdirsize, ' bytes in directory ', currentdir
                if ((thisdirsize >= missingdiskspace) .and. (thisdirsize < bestdeletedir)) then
                    bestdeletedir = thisdirsize
                end if
                return
            else if (line(1:4) == '$ cd') then
                ! deeper recursion on other '$ cd'
                thisdirsize = thisdirsize + calcdirsize(lines, currentstep, missingdiskspace, bestdeletedir)
            else
                ! if nothing else fit, the line must be a file and its size
                read(line, *) filesize, filename
                ! print *, 'file ', filename, ' has size ', filesize
                thisdirsize = thisdirsize + filesize
            end if
        end do
    end function calcdirsize

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)    :: filename
        integer, parameter              :: linebufferlength = 32
        character(len=:), allocatable   :: lines(:)
        integer                         :: totaldirsize, currentstep, missingdiskspace, bestdeletedir

        ! read whole file into an array of strings
        lines = readinputfile_asstringarray(filename, linebufferlength)

        ! traverse lines and calculate the total directory size
        currentstep = 1
        bestdeletedir = 0
        totaldirsize = calcdirsize(lines, currentstep, 0, bestdeletedir)  ! params 3 and 4 are ignored at first run
        ! print *, 'totaldirsize:', totaldirsize
        missingdiskspace = totaldirsize + 30000000 - 70000000
        ! print *, 'missingdiskspace:', missingdiskspace

        ! traverse lines again and determine best directory to delete
        currentstep = 1
        bestdeletedir = 70000000
        totaldirsize = calcdirsize(lines, currentstep, missingdiskspace, bestdeletedir)  ! params 3 and 4 ignored at first run
        ! print *, 'bestdeletedir:', bestdeletedir

        solve = bestdeletedir
    end function

end module day07b
