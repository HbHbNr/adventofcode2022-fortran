module day03a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_character_value
        use day03a, only : char_value
        implicit none

        call assert_equals (1, char_value('a'))
        call assert_equals (26, char_value('z'))
        call assert_equals (27, char_value('A'))
        call assert_equals (52, char_value('Z'))
    end subroutine

    subroutine test_find_duplicate
        use day03a, only : find_duplicate
        implicit none

        call assert_equals ('p', find_duplicate('vJrwpWtwJgWrhcsFMMfFFhFp'))
        call assert_equals ('L', find_duplicate('jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL'))
        call assert_equals ('P', find_duplicate('PmmdzqPrVvPwwTWBwg'))
        call assert_equals ('v', find_duplicate('wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn'))
        call assert_equals ('t', find_duplicate('ttgJtRGJQctTZtZT'))
        call assert_equals ('s', find_duplicate('CrZsJsPPZsGzwwsLwLmpwMDw'))
    end subroutine

    subroutine test_solve_example
        use day03a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day03_example.txt')
        call assert_equals (157, result)
    end subroutine

    subroutine test_solve_input
        use day03a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day03_input.txt')
        call assert_equals (7990, result)
    end subroutine

end module day03a_test
