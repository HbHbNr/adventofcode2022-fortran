module day12b_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_nodepriorityqueue
        use day12b, only : Position, Node, NodePriorityQueue, initvoidnode
        implicit none

        type(NodePriorityQueue) :: queue
        type(Node)              :: thenode

        call initvoidnode()
        call thenode%setvoid()

        call queue%init(10)
        call assert_equals (queue%empty(), .true.)
        call queue%add_or_update(thenode)
        call assert_equals (queue%empty(), .false.)
    end subroutine

    subroutine test_solve_example
        use day12b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day12_example.txt')
        call assert_equals (31, result)
    end subroutine

    subroutine test_solve_input
        use day12b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day12_input.txt')
        call assert_equals (534, result)
    end subroutine

end module day12b_test
