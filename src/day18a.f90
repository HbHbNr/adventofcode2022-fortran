!> Solution for https://adventofcode.com/2022/day/18 part a
module day18a
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 8

    type :: Grid3D
        private

        logical, allocatable :: space(:,:,:)
    contains
        procedure :: init       => grid3d_init
        procedure :: countwalls => grid3d_countwalls
        procedure :: print      => grid3d_print
    end type Grid3D

    public :: solve

contains

    subroutine grid3d_init(this, cubes)
        implicit none

        class(Grid3D), intent(inout) :: this
        character(len=*), intent(in) :: cubes(:)
        integer                      :: i, x, y, z, minx, miny, minz, maxx, maxy, maxz
        integer, parameter           :: border = 1

        ! find min and max coordinates
        read (cubes(1), *) x, y, z
        minx = x
        miny = y
        minz = z
        maxx = x
        maxy = y
        maxz = z
        do i = 1, size(cubes)
            read (cubes(i), *) x, y, z
            minx = min(minx, x)
            miny = min(miny, y)
            minz = min(minz, z)
            maxx = max(maxx, x)
            maxy = max(maxy, y)
            maxz = max(maxz, z)
        end do
        allocate(this%space(minx-border:maxx+border, miny-border:maxy+border, minz-border:maxz+border))
        this%space = .false.
        ! print *, minx, maxx, miny, maxy, minz, maxz

        ! fill space
        do i = 1, size(cubes)
            read (cubes(i), *) x, y, z
            this%space(x, y, z) = .true.
        end do
        call this%print(this%space)
    end subroutine

    function grid3d_countwalls(this) result(walls)
        implicit none

        class(Grid3D), intent(in) :: this
        integer                   :: walls
        logical, allocatable      :: tmpspace(:,:,:)
        integer                   :: z, y

        allocate(tmpspace, mold=this%space)
        tmpspace = .false.
        walls = 0
        do z = lbound(this%space, 3), ubound(this%space, 3) - 1
            ! print *, 'z=', z
            do y = lbound(this%space, 2), ubound(this%space, 2)
                tmpspace(:,y,z) = this%space(:,y,z) .neqv. this%space(:,y,z+1)
                walls = walls + count(tmpspace(:,y,z))
            end do
        end do
        do z = lbound(this%space, 3), ubound(this%space, 3)
            ! print *, 'z=', z
            do y = lbound(this%space, 2), ubound(this%space, 2) - 1
                tmpspace(:,y,z) = this%space(:,y,z) .neqv. this%space(:,y+1,z)
                walls = walls + count(tmpspace(:,y,z))
            end do
        end do
    end function grid3d_countwalls

    subroutine grid3d_print(this, space)
        implicit none

        class(Grid3D), intent(in) :: this
        logical, allocatable      :: space(:,:,:)
        integer                   :: x, y

        do x = lbound(space, 1), ubound(space, 1)
            do y = lbound(space, 2), ubound(space, 2)
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
        integer                       :: walls

        lines = readinputfile_asstringarray(filename, maxlinelength)
        call grid%init(lines)
        walls = grid%countwalls()

        solve = walls
    end function

end module day18a
