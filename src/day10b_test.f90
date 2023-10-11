module day10b_test
    use fruit
    implicit none

    ! all tests will be public

contains

    ! day10_example1.txt is not working with day10b
    ! subroutine test_solve_example1
    !     ...
    ! end subroutine test_solve_example1

    subroutine test_solve_example2
        use day10b, only : solve
        implicit none

        integer           :: i
        character(len=40) :: result(6)
        character(len=40) :: patterns(6) = [ &
            '##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ', &
            '###   ###   ###   ###   ###   ###   ### ', &
            '####    ####    ####    ####    ####    ', &
            '#####     #####     #####     #####     ', &
            '######      ######      ######      ####', &
            '#######       #######       #######     ' &
        ]

        result = solve('../inputfiles/day10_example2.txt')
        do i = 1, size(result)
            call assert_equals (patterns(i), result(i))
        end do
    end subroutine test_solve_example2

    subroutine test_solve_input
        use day10b, only : solve
        implicit none

        integer           :: i
        character(len=40) :: result(6)
        character(len=40) :: patterns(6) = [ &
            '###  #  # ###    ## ###  ###  #     ##  ', &
            '#  # # #  #  #    # #  # #  # #    #  # ', &
            '#  # ##   #  #    # ###  #  # #    #  # ', &
            '###  # #  ###     # #  # ###  #    #### ', &
            '# #  # #  #    #  # #  # #    #    #  # ', &
            '#  # #  # #     ##  ###  #    #### #  # ' &
        ]

        result = solve('../inputfiles/day10_input.txt')
        do i = 1, size(result)
            call assert_equals (patterns(i), result(i))
        end do
    end subroutine test_solve_input

end module day10b_test
