! -------------------------------------------------------------------------------------------------------------------------------- !
! [target]                                                                                                                         !
! test the non intrinsic functions contained in non intrinsic module `remainder_reinforced`                                        !
!                                                                                                                                  !
! [compile command]                                                                                                                !
! @GFORTRAN ^                                                                                                                      !
! -o test_20190825_01.exe ^                                                                                                        !
! ???\GitHub\Fortran\miniparts\remainder_reinforced.f08 ^                                                                          !
! ???\GitHub\Fortran\test\test_20190825_01.f08                                                                                     !
! -------------------------------------------------------------------------------------------------------------------------------- !

program test_20190825_01

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: remainder_reinforced

  ! variables for this <program>
  integer(INT64) :: a, b, p, sum, mul
  
  ! STEP.01
  ! set the values for test
  a   = 13_INT64
  b   = 17_INT64
  p   =  5_INT64
  sum = a + b
  mul = a * b

  ! STEP.02
  ! test & output the result of the target functions
  print *, sum, mod(a = sum, p = p), mod_sum(a = a, b = b, p = p)
  print *, mul, mod(a = mul, p = p), mod_mul(a = a, b = b, p = p)

end program test_20190825_01

! -------------------------------------------------------------------------------------------------------------------------------- !
! End of Source Code                                                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !
