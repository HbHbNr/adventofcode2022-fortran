module util_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_readinputfile_asline_example
        use util, only : readinputfile_asline
        implicit none

        character(len=:), allocatable :: line

        line = readinputfile_asline('../inputfiles/day06_example.txt')
        call assert_equals (32, len_trim(line))
        call assert_equals (32, len(line))
        call assert_equals ('zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw', line)
    end subroutine

    subroutine test_readinputfile_asline_input
        use util, only : readinputfile_asline
        implicit none

        character(len=:), allocatable :: line

        line = readinputfile_asline('../inputfiles/day06_input.txt')
        call assert_equals (4095, len_trim(line))
        call assert_equals (4095, len(line))
        call assert_equals ('gzbzwzjwwrfftfrrbvvcbcvcffpssvhh', line(:32))
    end subroutine

    subroutine test_readinputfile_asstringarray_example
        use util, only : readinputfile_asstringarray
        implicit none

        integer, parameter            :: linebufferlength = 32
        character(len=:), allocatable :: lines(:)

        lines = readinputfile_asstringarray('../inputfiles/day07_example.txt', linebufferlength)

        call assert_equals (14, len(lines))
        call assert_equals (23, size(lines))
    end subroutine

    subroutine test_readinputfile_asstringarray_input
        use util, only : readinputfile_asstringarray
        implicit none

        integer, parameter            :: linebufferlength = 32
        character(len=:), allocatable :: lines(:)

        lines = readinputfile_asstringarray('../inputfiles/day07_input.txt', linebufferlength)
        call assert_equals (19, len(lines))
        call assert_equals (1031, size(lines))
    end subroutine

    subroutine test_readinputfile_asintarray_example
        use iso_fortran_env, only : int8
        use util, only : readinputfile_asintarray
        implicit none

        integer, parameter           :: linebufferlength = 5
        integer(kind=1), allocatable :: intarray(:,:)
        integer                      :: testvalue, testvalues(5) = [3, 3, 5, 4, 9], i

        intarray = readinputfile_asintarray('../inputfiles/day08_example.txt', linebufferlength)

        call assert_equals (5, size(intarray, 1))
        call assert_equals (5, size(intarray, 2))
        do i = 1, size(testvalues)
            testvalue = intarray(4,i)
            call assert_equals (testvalues(i), testvalue)
        end do
    end subroutine

    subroutine test_readinputfile_asintarray_input
        use iso_fortran_env, only : int8
        use util, only : readinputfile_asintarray
        implicit none

        integer, parameter           :: linebufferlength = 99
        integer(kind=1), allocatable :: intarray(:,:)
        integer                      :: testvalue, testvalues(5) = [6, 6, 3, 6, 4], i

        intarray = readinputfile_asintarray('../inputfiles/day08_input.txt', linebufferlength)

        call assert_equals (99, size(intarray, 1))
        call assert_equals (99, size(intarray, 2))
        do i = 1, size(testvalues)
            testvalue = intarray(26 + i, 16)
            call assert_equals (testvalues(i), testvalue)
        end do
    end subroutine

end module util_test
