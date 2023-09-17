program tests

  ! Driver program for FRUIT unit tests in:
  ! src/class_charstack_test.f90

  ! Generated by FRUITPy.

  use fruit
  use charstack_test

  implicit none
  integer :: failed_count

  call init_fruit

  call run_test_case(test_charstack,"test_charstack")

  call get_failed_count(failed_count)
  call fruit_summary
  call fruit_finalize
  if (failed_count > 0) stop 1

end program tests