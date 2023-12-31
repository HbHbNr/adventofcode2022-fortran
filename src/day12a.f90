!> Solution for https://adventofcode.com/2022/day/12 part a
module day12a
    use iso_fortran_env, only : error_unit
    use util, only : readinputfile_asstringarray
    implicit none
    private

    type, public :: Position
        integer :: row, col
    contains
        procedure :: equals => position_equals
    end type

    type, public :: Node
        type(Position) :: ownpos
        type(Position) :: parentpos
        integer        :: fValue
        integer        :: gValue
    contains
        procedure :: setvoid => node_setvoid
    end type

    type, public :: NodePriorityQueue
        private

        type(Node), allocatable :: values(:)
        integer                 :: valueslength
    contains
        procedure :: init          => nodepriorityqueue_init
        procedure :: add_or_update => nodepriorityqueue_add_or_update
        procedure :: pop           => nodepriorityqueue_pop
        procedure :: empty         => nodepriorityqueue_empty
    end type

    class(Node), allocatable :: voidnode

    public :: solve, initvoidnode

contains

    subroutine nodepriorityqueue_init(this, queuesize)
        implicit none

        class(NodePriorityQueue), intent(inout) :: this
        integer, intent(in)                     :: queuesize

        if (queuesize < 1) then
            write (error_unit, *) 'NodePriorityQueue error for init(): queue size must be at least 1'
            stop
        end if
        allocate(this%values(queuesize), source=voidnode)
        this%valueslength = 0
    end subroutine

    subroutine nodepriorityqueue_add_or_update(this, addnode)
        implicit none

        class(NodePriorityQueue), intent(inout) :: this
        type(Node)                              :: addnode
        integer                                 :: found, bestplace, i

        if (this%valueslength < 1) then
            ! trivial case: queue was empty
            this%valueslength = 1
            this%values(this%valueslength) = addnode
        else
            ! the queue has at least one entry
            found = 0
            bestplace = 0
            ! scan all entries
            do i = 1, this%valueslength
                ! check if node is at current index
                if (addnode%ownpos%equals(this%values(i)%ownpos)) found = i
                ! check if best place for new or updated node is at current index
                if (addnode%fValue <= this%values(i)%fValue) bestplace = i
            end do
            if (found == 0) then  ! node is totally new
                if (this%valueslength >= size(this%values)) then
                    write (error_unit, *) 'NodePriorityQueue error for add_or_update(): queue full'
                    stop
                end if
                if (bestplace == 0) then
                    ! add new node to end of queue
                    this%valueslength = this%valueslength + 1
                    this%values(this%valueslength) = addnode
                else
                    ! move tail to make room for the new node
                    this%values(bestplace+1:this%valueslength+1) = &
                        this%values(bestplace:this%valueslength)
                    this%values(bestplace) = addnode
                    this%valueslength = this%valueslength + 1
                end if
            else if (addnode%fValue < this%values(found)%fValue) then
                ! node has been found, and the new f value is better
                if (found < bestplace) then  ! move node to back
                    this%values(found:bestplace-1) = this%values(found+1:bestplace)
                else if (found > bestplace) then  ! move node to front
                    this%values(bestplace+1:found) = this%values(bestplace:found-1)
                end if
                ! place node at either the same place or at the new gap
                this%values(bestplace) = addnode
            end if
        end if 
    end subroutine

    function nodepriorityqueue_pop(this) result(popnode)
        implicit none

        class(NodePriorityQueue), intent(inout) :: this
        type(Node)                              :: popnode

        if (this%valueslength < 1) then
            write (error_unit, *) 'NodePriorityQueue error for pop(): empty queue'
            stop
        else
            popnode = this%values(1)
            this%values(1:this%valueslength-1) = this%values(2:this%valueslength)
            this%values(this%valueslength) = voidnode
            this%valueslength = this%valueslength - 1
        end if
    end function

    function nodepriorityqueue_empty(this) result(empty)
        implicit none

        class(NodePriorityQueue), intent(in) :: this
        logical                              :: empty

        empty = (this%valueslength == 0)
    end function

    function position_equals(this, other) result(equals)
        implicit none

        class(Position), intent(in) :: this, other
        logical                     :: equals

        equals = this%row == other%row .and. this%col == other%col
        ! print *, this%row, other%row, this%col, other%col, equals
    end function

    subroutine node_setvoid(this)
        implicit none

        class(Node), intent(inout) :: this

        this%ownpos%row = -1
        this%ownpos%col = -1
        this%parentpos%row = -1
        this%parentpos%col = -1
        this%fValue = -1
        this%gValue = -1
    end subroutine

    subroutine initvoidnode()
        implicit none

        if (.not. allocated(voidnode)) allocate(voidnode)
        call voidnode%setvoid()
    end subroutine

    subroutine find_startposendpos(matrix, startpos, endpos)
        implicit none

        character(len=*), intent(in)  :: matrix(:)
        type(Position), intent(out)   :: startpos, endpos
        integer                       :: col, row
        ! character(len=len(matrix(1))) :: tmpstring

        do row = 1, size(matrix)
            col = index(matrix(row), 'S')
            if (col > 0) then
                startpos%row = row
                startpos%col = col
            end if
            col = index(matrix(row), 'E')
            if (col > 0) then
                endpos%row = row
                endpos%col = col
            end if
        end do
    end subroutine

    function distance(a, b) result(d)
        implicit none

        type(Position), intent(in) :: a, b
        integer                    :: diffrow, diffcol, d

        diffrow = abs(a%row - b%row)
        diffcol = abs(a%col - b%col)
        ! Manhattan distance
        d = diffrow + diffcol
        ! ??? ! count crossroad only once
        ! ??? if (diffrow /= 0 .and. diffcol /= 0) d = d - 1
    end function

    function reachable(frompos, topos, matrix) result(isreachable)
        implicit none

        type(Position), intent(in)   :: frompos, topos
        character(len=*), intent(in) :: matrix(:)
        logical                      :: isreachable
        character(len=1)             :: fromchar, tochar

        isreachable = .false.
        fromchar = matrix(frompos%row)(frompos%col:frompos%col)
        tochar = matrix(topos%row)(topos%col:topos%col)
        if (fromchar == 'S') fromchar = 'a'
        if (tochar == 'E') tochar = 'z'
        ! at max one step up is reachable
        if (ichar(fromchar) >= (ichar(tochar) - 1)) then
            isreachable = .true.
        ! else
        !     print *, ichar(fromchar), ichar(tochar)
        end if
        ! if (.not. isreachable) print *, topos, ' is not reachable'
    end function

    subroutine print_map(currentnode, openlist, closelist, endpos, matrix)
        implicit none

        type(Node), intent(in)                 :: currentnode
        type(NodePriorityQueue), intent(inout) :: openlist
        type(Node), intent(in)                 :: closelist(:,:)
        type(Position), intent(in)             :: endpos
        character(len=*), intent(in)           :: matrix(:)
        character(len=1)                       :: mapchar
        integer                                :: row, col

        do row = 1, size(closelist, 1)
            do col = 1, size(closelist, 2)
                ! mapchar = matrix(row)(col:col)
                if (currentnode%ownpos%row == row .and. currentnode%ownpos%col == col) then
                    mapchar = '+'
                else if (closelist(row,col)%fValue /= -1) then
                    mapchar = '*'
                else
                    ! mapchar = '.'
                    mapchar = matrix(row)(col:col)
                end if
                write (*, '(A1)', advance='no') mapchar
            end do
            print *
        end do
    end subroutine

    subroutine expand_currentnode(currentnode, openlist, closelist, endpos, matrix)
        implicit none

        type(Node), intent(in)                 :: currentnode
        type(NodePriorityQueue), intent(inout) :: openlist
        type(Node), intent(in)                 :: closelist(:,:)
        type(Position), intent(in)             :: endpos
        character(len=*), intent(in)           :: matrix(:)
        integer, parameter                     :: offsets(8) = [0, 1, 1, 0, 0, -1, -1, 0]
        type(Position)                         :: newpos
        integer                                :: i
        type(Node)                             :: newnode

        ! print *, 'expanding ', currentnode
        do i = 1, 7, 2
            newpos = currentnode%ownpos
            newpos%row = newpos%row + offsets(i)
            newpos%col = newpos%col + offsets(i+1)
            ! check if new position is inside of matrix bounds
            if (newpos%row >= 1 .and. newpos%row <= size(closelist,1) .and. &
                newpos%col >= 1 .and. newpos%col <= size(closelist,2)) then
                ! check if new position is not in closelist
                if (closelist(newpos%row,newpos%col)%fValue == -1) then
                    if (reachable(currentnode%ownpos, newpos, matrix)) then
                        newnode%ownpos = newpos
                        newnode%parentpos = currentnode%ownpos
                        newnode%gValue = currentnode%gValue + 1
                        newnode%fValue = newnode%gValue + distance(newpos, endpos)
                        ! print *, 'new node', newnode

                        call openlist%add_or_update(newnode)
                    end if
                end if
            end if
        end do
    end subroutine

    function find_shortestpath(matrix, startpos, endpos) result(shortestpath)
        implicit none

        character(len=*), intent(in) :: matrix(:)
        type(Position), intent(in)   :: startpos, endpos
        integer                      :: shortestpath
        type(NodePriorityQueue)      :: openlist
        type(Node), allocatable      :: closelist(:,:)
        type(Node)                   :: startnode, currentnode

        ! create 2-dimensional array of nodes as closelist, with the same dimensions as the matrix
        allocate(closelist(size(matrix,1),len(matrix(1))), source=voidnode)
        ! create NodePriorityQueue as openlist, with a size to keep all nodes from the matrix
        call openlist%init(size(closelist))

        startnode%ownpos = startpos
        startnode%parentpos = startpos
        startnode%fValue = 0
        startnode%gValue = 0

        shortestpath = -1
        call openlist%add_or_update(startnode)
        do while (.not. openlist%empty())
            currentnode = openlist%pop()
            ! print *, 'current pos:', currentnode%ownpos%row, currentnode%ownpos%col
            ! call print_map(currentnode, openlist, closelist, endpos, matrix)

            ! check if current node is at the end position
            if (currentnode%ownpos%equals(endpos)) then
                ! end node reached, current node has the length of the path
                shortestpath = currentnode%gValue
                exit
            else
                ! save current node to closelist
                closelist(currentnode%ownpos%row,currentnode%ownpos%col) = currentnode
                ! expand current node
                call expand_currentnode(currentnode, openlist, closelist, endpos, matrix)
            end if
        end do
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: matrix(:)
        type(Position)                :: startpos, endpos
        integer                       :: shortestpath

        call initvoidnode()

        ! read matrix from file as an array of strings
        matrix = readinputfile_asstringarray(filename, 200)

        ! find start and end position of the matrix
        call find_startposendpos(matrix, startpos, endpos)

        shortestpath = find_shortestpath(matrix, startpos, endpos)

        solve = shortestpath
    end function

end module day12a
