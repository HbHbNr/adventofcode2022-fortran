!> Solution for https://adventofcode.com/2021/day/12 part a
module day12a
    use iso_fortran_env, only : error_unit
    use util, only : readinputfile_asstringarray
    implicit none
    private

    type, public :: Position
        integer :: row, col
    end type

    type, public :: Node
        type(Position) :: own
        type(Position) :: parent
        integer        :: fValue
    contains
        procedure :: setvoid => node_setvoid
    end type

    type, public :: NodePriorityQueue
        private

        type(Node), allocatable :: values(:)
        integer                 :: valueslength
    contains
        procedure :: init  => nodepriorityqueue_init
        procedure :: add   => nodepriorityqueue_add
        procedure :: pop   => nodepriorityqueue_pop
        procedure :: empty => nodepriorityqueue_empty
    end type

    class(Node), allocatable :: voidnode

    public :: solve

contains

    subroutine nodepriorityqueue_init(this, queuesize)
        class(NodePriorityQueue), intent(inout) :: this
        integer, intent(in)                     :: queuesize

        allocate(this%values(queuesize), source=voidnode)
        this%valueslength = 0
    end subroutine

    subroutine nodepriorityqueue_add(this, addnode)
        class(NodePriorityQueue), intent(inout) :: this
        type(Node)                              :: addnode

        if (this%valueslength >= size(this%values)) then
            write (error_unit, *) 'NodePriorityQueue error for getFipoprst(): queue full'
            stop
        else
            this%valueslength = this%valueslength + 1
            this%values(this%valueslength) = addnode
        end if
    end subroutine

    function nodepriorityqueue_pop(this) result(popnode)
        class(NodePriorityQueue), intent(inout) :: this
        type(Node)                              :: popnode

        if (this%valueslength < 1) then
            write (error_unit, *) 'NodePriorityQueue error for getFipoprst(): empty queue'
            stop
        else
            popnode = this%values(1)
            this%values(1:this%valueslength-1) = this%values(2:this%valueslength)
            this%values(this%valueslength) = voidnode
            this%valueslength = this%valueslength - 1
        end if
    end function

    function nodepriorityqueue_empty(this) result(empty)
        class(NodePriorityQueue), intent(in) :: this
        logical                              :: empty

        empty = (this%valueslength == 0)
    end function

    subroutine node_setvoid(this)
        class(Node), intent(inout) :: this

        this%own%row = -1
        this%own%col = -1
        this%parent%row = -1
        this%parent%col = -1
        this%fValue = -1
    end subroutine

    subroutine initvoidnode()
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

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: matrix(:)
        logical(kind=1), allocatable  :: closelist(:,:)
        type(Position)                :: startpos, endpos

        call initvoidnode()

        ! read matrix from file as an array of strings
        matrix = readinputfile_asstringarray(filename, 200)
        ! create 2-dimensional array of logicals of the same size
        allocate(closelist(size(matrix),len(matrix(1))))

        call find_startposendpos(matrix, startpos, endpos)
        print *, startpos
        print *, endpos
        print *, distance(startpos, endpos)

        ! return length of shortest path
        solve = -1
    end function

end module day12a
