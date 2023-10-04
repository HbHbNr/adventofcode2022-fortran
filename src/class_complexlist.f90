module class_complexlist
    use iso_fortran_env, only : error_unit
    implicit none
    private

    type, public :: ComplexList
        private

        complex, allocatable :: values(:)
        integer              :: valueslength
    contains
        procedure :: init         => complexlist_init
        procedure :: add          => complexlist_add
        procedure :: get          => complexlist_get
        procedure :: empty        => complexlist_empty
        procedure :: length       => complexlist_length
        procedure :: containsints => complexlist_containsints
    end type ComplexList

contains

    subroutine complexlist_init(this, listsize)
        implicit none

        class(ComplexList), intent(inout) :: this
        integer, intent(in)               :: listsize

        allocate(this%values(listsize), source=(0,0))
        this%valueslength = 0
    end subroutine

    subroutine complexlist_add(this, value)
        implicit none

        class(ComplexList), intent(inout) :: this
        complex, intent(in)               :: value

        this%valueslength = this%valueslength + 1
        this%values(this%valueslength) = value
    end subroutine

    function complexlist_get(this, index, defaultvalue) result(value)
        implicit none

        class(ComplexList), intent(in) :: this
        integer, intent(in)            :: index
        integer, optional, intent(in)  :: defaultvalue
        complex                        :: value

        if ((index < 1) .or. (index > this%valueslength)) then
            if (present(defaultvalue)) then
                value = defaultvalue
            else
                if (this%valueslength == 0) then
                    write (error_unit, *) 'List error: index ', index, ' out of bounds (empty list)'
                else
                    write (error_unit, *) 'List error: index ', index, ' out of bounds (1:', this%valueslength, ')'
                end if
                stop
            end if
        else
            value = this%values(index)
        end if
    end function

    pure function complexlist_empty(this) result(empty)
        implicit none

        class(ComplexList), intent(in) :: this
        logical                        :: empty

        empty = (this%valueslength == 0)
    end function

    pure function complexlist_length(this) result(count)
        implicit none

        class(ComplexList), intent(in) :: this
        integer                        :: count

        count = this%valueslength
    end function

    pure function complexlist_containsints(this, value) result(contains)
        implicit none

        class(ComplexList), intent(in) :: this
        complex, intent(in)            :: value
        logical                        :: contains
        integer                        :: i
        complex                        :: item

        contains = .false.
        if (this%valueslength > 0) then
            do i = 1, this%valueslength
                item = this%values(i)
                if (int(real(value)) == int(real(item))) then
                    if (int(aimag(value)) == int(aimag(item))) then
                        contains = .true.
                        exit
                    end if
                end if
            end do
        end if
    end function

end module class_complexlist
