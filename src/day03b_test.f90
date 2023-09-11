module day03b_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_character_value
        use day03b, only : char_value
        implicit none

        call assert_equals (1, char_value('a'))
        call assert_equals (26, char_value('z'))
        call assert_equals (27, char_value('A'))
        call assert_equals (52, char_value('Z'))
    end subroutine

    subroutine test_find_badge
        use day03b, only : find_badge
        implicit none

        integer :: badge

        badge = find_badge('vJrwpWtwJgWrhcsFMMfFFhFp', 'jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL', 'PmmdzqPrVvPwwTWBwg')
        call assert_equals (18, badge)
        badge = find_badge('wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn', 'ttgJtRGJQctTZtZT', 'CrZsJsPPZsGzwwsLwLmpwMDw')
        call assert_equals (52, badge)
    end subroutine

    subroutine test_solve_example
        use day03b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day03_example.txt')
        call assert_equals (70, result)
    end subroutine

    subroutine test_solve_input
        use day03b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day03_input.txt')
        call assert_equals (2602, result)
    end subroutine

end module day03b_test
