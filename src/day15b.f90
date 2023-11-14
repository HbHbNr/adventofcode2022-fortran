!> Solution for https://adventofcode.com/2021/day/15 part b
module day15b
    use iso_fortran_env, only : error_unit
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 80
    character(len=1), parameter :: char_empty = '.', char_sensor = 'S', char_beacon = 'B', char_coverage = '#'

    type :: SBMap
        private

        ! character(len=:), allocatable :: map
        integer                       :: minx, maxx, miny, maxy
        ! character(len=:), allocatable :: fillline
    contains
        procedure :: init         => sbmap_init
        ! procedure :: put          => sbmap_put
        ! procedure :: get          => sbmap_get
        ! procedure :: isfree       => sbmap_isfree
        procedure :: isvalid      => sbmap_isvalid
        ! procedure :: drawcoverage => sbmap_drawcoverage
        ! procedure :: print        => sbmap_print
    end type SBMap

    public :: solve

contains

    subroutine sbmap_init(this, maxx, yline)
        implicit none

        class(SBMap), intent(inout)   :: this
        integer, intent(in)           :: maxx, yline

        ! calculate dimensions of map
        this%minx = 0
        this%maxx = maxx
        this%miny = yline
        this%maxy = yline
        ! print *, this%minx, this%maxx, this%miny, this%maxy
    end subroutine

    ! subroutine sbmap_put(this, char, x, y)
    !     implicit none

    !     class(SBMap), intent(inout)  :: this
    !     character(len=1), intent(in) :: char
    !     integer, intent(in)          :: x, y
    !     integer                      :: xfixed

    !     xfixed = x - this%minx + 1
    !     this%map(xfixed:xfixed) = char
    ! end subroutine

    ! function sbmap_get(this, x, y) result(char)
    !     implicit none

    !     class(SBMap), intent(inout) :: this
    !     integer, intent(in)         :: x, y
    !     character(len=1)            :: char
    !     integer                     :: xfixed

    !     xfixed = x - this%minx + 1
    !     char = this%map(xfixed:xfixed)
    ! end function

    ! function sbmap_isfree(this, x, y) result(isfree)
    !     implicit none

    !     class(SBMap), intent(inout) :: this
    !     integer, intent(in)         :: x, y
    !     logical                     :: isfree

    !     if (y /= this%yline) then
    !         ! every non-interesting line is free
    !         isfree = .true.
    !     else
    !         isfree = this%get(x, y) == char_empty
    !     end if
    ! end function

    function sbmap_isvalid(this, x, y) result(isvalid)
        implicit none

        class(SBMap), intent(inout) :: this
        integer, intent(in)           :: x, y
        logical                       :: isvalid

        isvalid = x >= this%minx .and. x <= this%maxx .and. y >= this%miny .and. y <= this%maxy
    end function

    ! subroutine sbmap_drawcoverage(this, coords)
    !     implicit none

    !     class(SBMap), intent(inout) :: this
    !     integer, intent(in)         :: coords(:)
    !     integer                     :: i, sensorx, sensory, beaconx, beacony, distance, x, y

    !     this%map(:) = this%fillline
    !     do i = 1, size(coords), 4
    !         sensorx = coords(i)
    !         sensory = coords(i+1)
    !         beaconx = coords(i+2)
    !         beacony = coords(i+3)
    !         distance = abs(beaconx-sensorx) + abs(beacony-sensory)
    !         if (sensory + distance < this%miny .or. sensory - distance > this%maxy) then
    !             ! whole coverage is out of valid y range
    !             cycle
    !         end if
    !         if (sensorx + distance < this%minx .or. sensorx - distance > this%maxx) then
    !             ! whole coverage is out of valid x range
    !             cycle
    !         end if
    !         do y = -distance, distance
    !             if (sensory + y < this%miny .or. sensory + y > this%maxy) then
    !                 ! whole line is out of valid y range
    !                 cycle
    !             end if
    !             do x = -(distance-abs(y)), distance-abs(y)
    !                 if (sensorx + x < this%minx .or. sensorx + x > this%maxx) then
    !                     ! this position is out of valid x range
    !                     cycle
    !                 end if
    !                 call this%put(char_coverage, sensorx + x, sensory + y)
    !             end do
    !         end do
    !     end do
    !     ! restore sensors and beacons
    !     do i = 1, size(coords), 4
    !         ! sensorx = coords(i)
    !         ! sensory = coords(i+1)
    !         beaconx = coords(i+2)
    !         beacony = coords(i+3)
    !         ! call this%put(char_sensor, sensorx, sensory)
    !         if (this%isvalid(beaconx, beacony)) then
    !             call this%put(char_beacon, beaconx, beacony)
    !         end if
    !     end do
    !     ! print *
    !     ! print '(I2, A, A)', this%yline, ' ', this%map(this%yline)
    ! end subroutine

    ! subroutine sbmap_print(this)
    !     implicit none

    !     class(SBMap), intent(inout) :: this
    !     integer                     :: y

    !     do y = this%miny, this%maxy
    !         print '(I2, A, A)', y, ' ', this%map
    !     end do
    ! end subroutine

    subroutine extract_coords(lines, coords)
        character(len=*), intent(in)      :: lines(:)
        integer, intent(out), allocatable :: coords(:)
        character(len=maxlinelength)      :: line
        character(len=1)                  :: char
        integer                           :: i, j, c, ichar0 = ichar('0'), ichar9 = ichar('9')

        ! 5 coords per line
        allocate(coords(size(lines) * 5))

        ! scan lines an save coords
        c = 0  ! number of single coords saved into linear array
        do i = 1, size(lines)
            line = lines(i)
            do j = 1, len_trim(line)
                char = line(j:j)
                if (char /= '-') then
                    if (ichar(char) < ichar0 .or. ichar(char) > ichar9) then
                        line(j:j) = ' '
                    end if
                end if
            end do

            ! read coords into the correct places of the array
            read (line, *) coords(c + 1:c + 4)

            ! add distance between sensor and beacon as 5th coordinate
            coords(c + 5) = abs(coords(c + 3)-coords(c + 1)) + abs(coords(c + 4)-coords(c + 2))

            c = c + 5
        end do
    end subroutine

    ! function find_tuning_frequency(map) result(tuning_frequency)
    !     implicit none

    !     type(SBMap), intent(inout) :: map
    !     integer                    :: tuning_frequency
    !     integer                    :: x, y, xfixed, linelength

    !     ! linelength = len(map%map(map%miny))
    !     ! yloop: do y = map%miny, map%maxy
    !     !     do x = 1, linelength
    !     !         if (map%map(y)(x:x) == char_empty) then
    !     !             exit yloop
    !     !         end if
    !     !     end do
    !     ! end do yloop
    !     x = scan(map%map, char_empty)
    !     ! print *, x
    !     if (x == 0) then
    !         tuning_frequency = 0
    !     else
    !         xfixed = map%minx + x - 1
    !         tuning_frequency = xfixed * 4000000 + map%yline
    !     end if
    ! end function

    integer function solve(filename, maxcoord)
        implicit none

        character(len=*), intent(in)  :: filename
        integer, intent(in)           :: maxcoord
        character(len=:), allocatable :: lines(:)
        integer, allocatable          :: coords(:)
        type(SBMap)                   :: map
        integer                       :: yline
        integer                       :: tuning_frequency

        lines = readinputfile_asstringarray(filename, maxlinelength)

        call extract_coords(lines, coords)
        print *, coords

        ! call map%init(maxcoord, yline)
        ! do yline = 0, maxcoord
        !     print *, yline
        !     map%miny = yline
        !     map%maxy = yline
        !     map%yline = yline
        !     ! mark sensors and beacons
        !     call map%drawcoverage(coords)
        !     ! call map%print()
        !     tuning_frequency = find_tuning_frequency(map)
        !     if (tuning_frequency /= 0) then
        !         exit
        !     end if
        ! end do
        tuning_frequency = -1

        solve = tuning_frequency
    end function

end module day15b
