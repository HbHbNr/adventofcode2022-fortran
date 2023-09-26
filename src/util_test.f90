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

    subroutine test_readinputfile_asarray_example
        use util, only : readinputfile_asarray
        implicit none

        integer, parameter            :: linebufferlength = 32
        character(len=:), allocatable :: lines(:)

        lines = readinputfile_asarray('../inputfiles/day07_example.txt', linebufferlength)

        call assert_equals (14, len(lines))
        call assert_equals (23, size(lines))
    end subroutine

    subroutine test_readinputfile_asarray_input
        use util, only : readinputfile_asarray
        implicit none

        integer, parameter            :: linebufferlength = 32
        character(len=:), allocatable :: lines(:)

        lines = readinputfile_asarray('../inputfiles/day07_input.txt', linebufferlength)
        call assert_equals (19, len(lines))
        call assert_equals (1031, size(lines))
    end subroutine

end module util_test
