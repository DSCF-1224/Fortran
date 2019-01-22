! ------------------------------------------------------------------------------------------------------------------------------- !
! [Project Euler] 0006                                                                                                            !
! https://projecteuler.net/problem=6                                                                                              !
! http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%206                                                        !
! ------------------------------------------------------------------------------------------------------------------------------- !
program main

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: Problem0006

  ! Require all variables to be explicitly declared
  implicit none

  ! variables for this <program>
  integer( kind= INT64 ) :: itr, target

  do itr = 1, 7, 1
    target = 10_INT64 ** itr
    write( unit=output_unit, fmt='(I3,I10,I30)', advance='yes' ) itr, target, Problem0006_02( target )
  end do
  
end program main
! ------------------------------------------------------------------------------------------------------------------------------- !
! gfortran ^                                                                                                                      !
! -c ^                                                                                                                            !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! D:\GitHub\Fortran\ProjectEuler\Problem0006\Problem0006_01.f08 ^                                                                 !
! D:\GitHub\Fortran\ProjectEuler\Problem0006\main.f08                                                                             !
!                                                                                                                                 !
! gfortran ^                                                                                                                      !
! -o Problem0006_01.exe ^                                                                                                         !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! D:\gfortran\Problem0006_01.o ^                                                                                                  !
! D:\gfortran\main.o                                                                                                              !
! ------------------------------------------------------------------------------------------------------------------------------- !