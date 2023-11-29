!> Solution for https://adventofcode.com/2022/day/18 part b
module day18b
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 8
    integer, parameter :: border = 1

    type :: Grid3D
        private

        logical, allocatable :: space(:,:,:)
        integer              :: minx, miny, minz, maxx, maxy, maxz
    contains
        procedure :: init             => grid3d_init
        procedure :: countwalls       => grid3d_countwalls
        procedure :: floodspace       => grid3d_floodspace
        procedure :: countbubblewalls => grid3d_countbubblewalls
        procedure :: print            => grid3d_print
    end type Grid3D

    public :: solve

contains

    subroutine grid3d_init(this, cubes)
        implicit none

        class(Grid3D), intent(inout) :: this
        character(len=*), intent(in) :: cubes(:)
        integer                      :: i, x, y, z

        ! find min and max coordinates
        read (cubes(1), *) x, y, z
        this%minx = x
        this%miny = y
        this%minz = z
        this%maxx = x
        this%maxy = y
        this%maxz = z
        do i = 1, size(cubes)
            read (cubes(i), *) x, y, z
            this%minx = min(this%minx, x)
            this%miny = min(this%miny, y)
            this%minz = min(this%minz, z)
            this%maxx = max(this%maxx, x)
            this%maxy = max(this%maxy, y)
            this%maxz = max(this%maxz, z)
        end do

        ! allocate and erase 3d grid
        allocate(this%space(this%minx-border:this%maxx+border, &
                            this%miny-border:this%maxy+border, &
                            this%minz-border:this%maxz+border))
        this%space = .false.

        ! fill space
        do i = 1, size(cubes)
            read (cubes(i), *) x, y, z
            this%space(x, y, z) = .true.
        end do
    end subroutine

    function grid3d_countwalls(this) result(walls)
        implicit none

        class(Grid3D), intent(in) :: this
        integer                   :: walls
        logical, allocatable      :: tmpspace(:,:,:)
        integer                   :: z, y, x

        ! allocate second grid of the same size to store the results
        allocate(tmpspace, mold=this%space)
        walls = 0

        ! compare each x-column with the neighbour on the next z-level
        tmpspace = .false.
        do z = this%minz - border, this%maxz
            do y = this%miny, this%maxy
                tmpspace(:,y,z) = this%space(:,y,z) .neqv. this%space(:,y,z+1)
            end do
        end do
        ! each .true. is a detected wall
        walls = walls + count(tmpspace)

        ! compare each x-column with the neighbour on the next y-level
        tmpspace = .false.
        do z = this%minz, this%maxz
            do y = this%miny - border, this%maxy
                tmpspace(:,y,z) = this%space(:,y,z) .neqv. this%space(:,y+1,z)
            end do
        end do
        ! each .true. is a detected wall
        walls = walls + count(tmpspace)

        ! within each x-column, compare each cube with the neighbour on the next x-level
        do z = this%minz, this%maxz
            do y = this%miny, this%maxy
                do x = this%minx - border, this%maxx
                    if (this%space(x,y,z) .neqv. this%space(x+1,y,z)) then
                        walls = walls + 1
                    end if
                end do
            end do
        end do
    end function

    recursive subroutine grid3d_floodspace(this, space, x, y, z)
        implicit none

        class(Grid3D), intent(in)           :: this
        logical, allocatable, intent(inout) :: space(:,:,:)
        integer, intent(in)                 :: x, y, z

        ! fill current cube
        space(x, y, z) = .true.

        ! recurse into empty neighbour cubes
        if (x > this%minx-border) then
            if (.not. space(x-1, y, z)) then
                call this%floodspace(space, x-1, y, z)
            end if
        end if
        if (x < this%maxx+border) then
            if (.not. space(x+1, y, z)) then
                call this%floodspace(space, x+1, y, z)
            end if
        end if
        if (y > this%miny-border) then
            if (.not. space(x, y-1, z)) then
                call this%floodspace(space, x, y-1, z)
            end if
        end if
        if (y < this%maxy+border) then
            if (.not. space(x, y+1, z)) then
                call this%floodspace(space, x, y+1, z)
            end if
        end if
        if (z > this%minz-border) then
            if (.not. space(x, y, z-1)) then
                call this%floodspace(space, x, y, z-1)
            end if
        end if
        if (z < this%maxz+border) then
            if (.not. space(x, y, z+1)) then
                call this%floodspace(space, x, y, z+1)
            end if
        end if
    end subroutine

    function grid3d_countbubblewalls(this) result(allbubblewalls)
        implicit none

        class(Grid3D), intent(in) :: this
        integer                   :: allbubblewalls, singlebubblewalls
        logical, allocatable      :: tmpspace(:,:,:)
        integer                   :: z, y, x

        ! allocate second grid as a copy the space
        allocate(tmpspace, source=this%space)
        ! flood temporary space
        call this%floodspace(tmpspace, this%minx-border, this%miny-border, this%minz-border)

        allbubblewalls = 0

        ! scan flooded space for air bubbles, which are never at the outside, and count their walls
        do z = this%minz + 1, this%maxz - 1
            do y = this%miny + 1, this%maxy - 1
                do x = this%minx + 1, this%maxx - 1
                    if (.not. tmpspace(x,y,z)) then
                        singlebubblewalls = 0
                        if (tmpspace(x-1,y,z)) singlebubblewalls = singlebubblewalls + 1
                        if (tmpspace(x+1,y,z)) singlebubblewalls = singlebubblewalls + 1
                        if (tmpspace(x,y-1,z)) singlebubblewalls = singlebubblewalls + 1
                        if (tmpspace(x,y+1,z)) singlebubblewalls = singlebubblewalls + 1
                        if (tmpspace(x,y,z-1)) singlebubblewalls = singlebubblewalls + 1
                        if (tmpspace(x,y,z+1)) singlebubblewalls = singlebubblewalls + 1
                        allbubblewalls = allbubblewalls + singlebubblewalls
                    end if
                end do
            end do
        end do
    end function

    subroutine grid3d_print(this, space)
        implicit none

        class(Grid3D), intent(in) :: this
        logical, allocatable      :: space(:,:,:)
        integer                   :: x, y

        do x = lbound(space, 1), ubound(space, 1)
            do y = lbound(space, 2), ubound(space, 2)
                print *, x, y
                print *, space(x,y,:)
            end do
            print *
        end do
    end subroutine

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        type(Grid3D)                  :: grid
        integer                       :: walls, bubblewalls

        lines = readinputfile_asstringarray(filename, maxlinelength)
        call grid%init(lines)
        walls = grid%countwalls()
        bubblewalls = grid%countbubblewalls()

        solve = walls - bubblewalls
    end function

end module day18b
