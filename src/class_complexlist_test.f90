module complexlist_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_charstack
        use class_ComplexList, only : ComplexList
        implicit none

        type(ComplexList) :: list
        ! logical           :: b
        complex           :: c

        call list%init(16)
        call assert_equals(.true., list%empty())
        call list%add(cmplx(1,2))
        call list%add(cmplx(3,4))
        call assert_equals(.false., list%empty())
        call assert_equals(2, list%count())
        call list%add(cmplx(5,6))
        call list%add(cmplx(7,8))
        call assert_equals(4, list%count())
        c = list%get(3)
        call assert_equals(5, int(real(c)))
        call assert_equals(6, int(aimag(c)))
    end subroutine

end module complexlist_test
