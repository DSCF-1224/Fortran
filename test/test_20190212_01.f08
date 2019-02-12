!----------------------------------------------------------------------------------------------------------------------------------!
! [target]                                                                                                                         !
! How to use NaN on gfortran                                                                                                          !
! [reference]                                                                                                                      !
! https://qiita.com/implicit_none/items/a96d5bdb1a0cf05b6f03#fnref3                                                                 !
!----------------------------------------------------------------------------------------------------------------------------------!
program main

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! require all variables to be explicitly declared
  implicit none

  real( kind= REAL64 ), parameter :: NaN = TRANSFER( -1_INT64, 0.0_REAL64 )

  PRINT *, NaN
  PRINT *, '[END]'
  READ *

end program main