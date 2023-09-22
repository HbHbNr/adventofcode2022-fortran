module day06a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_unique_string
        use day06a, only : unique_string
        implicit none

        call assert_equals (.true., unique_string('abcd'))
        call assert_equals (.false., unique_string('abad'))
    end subroutine

    subroutine test_find_sopmarker
        use day06a, only : find_sopmarker
        implicit none

        call assert_equals (5, find_sopmarker('bvwbjplbgvbhsrlpgdmjqwftvncz'))
        call assert_equals (6, find_sopmarker('nppdvjthqldpwncqszvftbrmjlhg'))
        call assert_equals (10, find_sopmarker('nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg'))
        call assert_equals (11, find_sopmarker('zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw'))
    end subroutine

    subroutine test_solve_example
        use day06a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day06_example.txt')
        call assert_equals (11, result)
    end subroutine

    subroutine test_solve_input
        use day06a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day06_input.txt')
        call assert_equals (1262, result)
    end subroutine

end module day06a_test
