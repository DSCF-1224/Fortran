! ------------------------------------------------------------------------------------------------------------------------------- !
! [Project Euler] 0005                                                                                                            !
! https://projecteuler.net/problem=5                                                                                              !
! http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%205                                                        !
! ------------------------------------------------------------------------------------------------------------------------------- !
program main

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: Problem0005

  ! Require all variables to be explicitly declared
  implicit none

  ! variables for this <program>
  integer( kind=INT64 ) :: num

  write( unit=output_unit, fmt='(A)', advance='yes' ) '[input the numbers]'
  read *, num

  print *, num, Problem0005_01( num )
  read *
  
end program main
! ------------------------------------------------------------------------------------------------------------------------------- !
! gfortran ^                                                                                                                      !
! -c ^                                                                                                                            !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! D:\GitHub\Fortran\miniparts\GCD_LCM.f08 ^                                                                                       !
! D:\GitHub\Fortran\ProjectEuler\Problem0005\Problem0005_01.f08 ^                                                                 !
! D:\GitHub\Fortran\ProjectEuler\Problem0005\main.f08                                                                             !
!                                                                                                                                 !
! gfortran ^                                                                                                                      !
! -o Problem0005_01.exe ^                                                                                                         !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! GCD_LCM.o ^                                                                                                                     !
! D:\gfortran\Problem0005_01.o ^                                                                                                  !
! D:\gfortran\main.o                                                                                                              !
! ------------------------------------------------------------------------------------------------------------------------------- !