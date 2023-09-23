module day06b_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_unique_string
        use day06b, only : unique_string
        implicit none

        call assert_equals (.true., unique_string('abcd'))
        call assert_equals (.false., unique_string('abad'))
    end subroutine

    subroutine test_find_sopmarker
        use day06b, only : find_sopmarker
        implicit none

        call assert_equals (19, find_sopmarker('mjqjpqmgbljsphdztnvjfqwrcgsmlb'))
        call assert_equals (23, find_sopmarker('bvwbjplbgvbhsrlpgdmjqwftvncz'))
        call assert_equals (23, find_sopmarker('nppdvjthqldpwncqszvftbrmjlhg'))
        call assert_equals (29, find_sopmarker('nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg'))
        call assert_equals (26, find_sopmarker('zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw'))
    end subroutine

    subroutine test_solve_example
        use day06b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day06_example.txt')
        call assert_equals (26, result)
    end subroutine

    subroutine test_solve_input
        use day06b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day06_input.txt')
        call assert_equals (3444, result)
    end subroutine

end module day06b_test
