module util_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_printarray
        use iso_fortran_env, only : int64
        use util, only : printarray
        implicit none

        integer          :: array1(5) = [1, 2, 3, 4, 5]
        integer          :: array2(3) = [6, 7, 8]
        integer(int64)   :: array3(3) = [60_int64, 70_int64, 80_int64]
        character(len=3) :: array4(3) = ['600', '700', '800']

        call printarray(array1)
        call printarray(array2)
        call printarray(array1, array2)
        call printarray(array3)
        call printarray(array4)
    end subroutine

    subroutine test_printresultline_integer
        use util, only : printresultline_integer
        implicit none

        integer :: i = 42

        call printresultline_integer('01a', i)
    end subroutine

    subroutine test_printresultline_int64
        use iso_fortran_env, only : int64
        use util, only : printresultline_int64
        implicit none

        integer(int64) :: i = 42_int64

        call printresultline_int64('01a', i)
    end subroutine

    subroutine test_printresultline_stringarray
        use util, only : printresultline_stringarray
        implicit none

        character(len=5) :: s(3) = ['1    ', '2    ', '42   ']

        call printresultline_stringarray('01a', s)
    end subroutine

    subroutine test_printresultline
        use util, only : printresultline
        implicit none

        character(len=5) :: s = '42'

        call printresultline('01a', s)
    end subroutine

    ! printioerror(...) is not testable, because it will stop the program

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
        integer(int8), allocatable   :: intarray(:,:)
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
        integer(int8), allocatable   :: intarray(:,:)
        integer                      :: testvalue, testvalues(5) = [6, 6, 3, 6, 4], i

        intarray = readinputfile_asintarray('../inputfiles/day08_input.txt', linebufferlength)

        call assert_equals (99, size(intarray, 1))
        call assert_equals (99, size(intarray, 2))
        do i = 1, size(testvalues)
            testvalue = intarray(26 + i, 16)
            call assert_equals (testvalues(i), testvalue)
        end do
    end subroutine

    subroutine test_string_extract_int64s
        use iso_fortran_env, only : int64
        use util, only : string_extract_int64s
        implicit none

        character(len=*), parameter :: string = '1716002126 3982609232 32819234'
        integer(int64), allocatable   :: seeds(:)

        call string_extract_int64s(string, seeds)

        call assert_true (seeds(1) == 1716002126_int64)
        call assert_true (seeds(2) == 3982609232_int64)
        call assert_true (seeds(3) == 32819234_int64)
    end subroutine

    subroutine test_string_extract_integers
        use util, only : string_extract_integers
        implicit none

        character(len=*), parameter :: string = '79 14 55 13'
        integer, allocatable   :: seeds(:)

        call string_extract_integers(string, seeds)

        call assert_true (seeds(1) == 79)
        call assert_true (seeds(2) == 14)
        call assert_true (seeds(3) == 55)
        call assert_true (seeds(4) == 13)
    end subroutine

end module util_test
