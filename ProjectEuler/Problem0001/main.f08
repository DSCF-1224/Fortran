! ------------------------------------------------------------------------------------------------------------------------------- !
! [Project Euler] 0001                                                                                                            !
! https://projecteuler.net/problem=1                                                                                              !
! http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%201                                                        !
! ------------------------------------------------------------------------------------------------------------------------------- !
program main

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: Problem0001

  ! Require all variables to be explicitly declared
  implicit none

  write( unit=output_unit, fmt='(I20)', advance='yes' ) Problem0001_03(     10_INT64 )
  write( unit=output_unit, fmt='(I20)', advance='yes' ) Problem0001_03(    100_INT64 )
  write( unit=output_unit, fmt='(I20)', advance='yes' ) Problem0001_03(   1000_INT64 )
  write( unit=output_unit, fmt='(I20)', advance='yes' ) Problem0001_03(  10000_INT64 )
  write( unit=output_unit, fmt='(I20)', advance='yes' ) Problem0001_03( 100000_INT64 )
  read *
  
end program main
! ------------------------------------------------------------------------------------------------------------------------------- !
! gfortran ^                                                                                                                      !
! -c ^                                                                                                                            !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! D:\GitHub\Fortran\ProjectEuler\Problem0001\Problem0001_01.f08 ^                                                                 !
! D:\GitHub\Fortran\ProjectEuler\Problem0001\main.f08                                                                             !
!                                                                                                                                 !
! gfortran ^                                                                                                                      !
! -o Problem0001_01.exe ^                                                                                                         !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! D:\gfortran\Problem0001_01.o ^                                                                                                  !
! D:\gfortran\main.o                                                                                                              !
! ------------------------------------------------------------------------------------------------------------------------------- !