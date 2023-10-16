program tests

  ! Driver program for FRUIT unit tests in:
  ! src/util_test.f90

  ! Generated by FRUITPy.

  use fruit
  use util_test

  implicit none
  integer :: failed_count

  call init_fruit

  call run_test_case(test_printarray_integer,"test_printarray_integer")
  call run_test_case(test_readinputfile_asline_example,"test_readinputfile_asline_example")
  call run_test_case(test_readinputfile_asline_input,"test_readinputfile_asline_input")
  call run_test_case(test_readinputfile_asstringarray_example,"test_readinputfile_asstringarray_example")
  call run_test_case(test_readinputfile_asstringarray_input,"test_readinputfile_asstringarray_input")
  call run_test_case(test_readinputfile_asintarray_example,"test_readinputfile_asintarray_example")
  call run_test_case(test_readinputfile_asintarray_input,"test_readinputfile_asintarray_input")

  call get_failed_count(failed_count)
  call fruit_summary
  call fruit_finalize
  if (failed_count > 0) stop 1

end program tests