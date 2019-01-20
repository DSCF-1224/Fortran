! ------------------------------------------------------------------------------------------------------------------------------- !
! [Project Euler] 0004                                                                                                            !
! https://projecteuler.net/problem=2                                                                                              !
! http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%202                                                        !
! ------------------------------------------------------------------------------------------------------------------------------- !
program main

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: Problem0004

  ! Require all variables to be explicitly declared
  implicit none

  ! variables for this <program>
  integer( kind=INT64 ) :: digit

  write( unit=output_unit, fmt='(A)', advance='yes' ) '[input the digits of the factor]'
  read *, digit

  call Problem0004_01( digit )
  read *
  
end program main
! ------------------------------------------------------------------------------------------------------------------------------- !
! gfortran ^                                                                                                                      !
! -c ^                                                                                                                            !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! D:\GitHub\Fortran\ProjectEuler\Problem0004\Problem0004_01.f08 ^                                                                 !
! D:\GitHub\Fortran\ProjectEuler\Problem0004\main.f08                                                                             !
!                                                                                                                                 !
! gfortran ^                                                                                                                      !
! -o Problem0004_01.exe ^                                                                                                         !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! D:\gfortran\Problem0004_01.o ^                                                                                                  !
! D:\gfortran\main.o                                                                                                              !
! ------------------------------------------------------------------------------------------------------------------------------- !