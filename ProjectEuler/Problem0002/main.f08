! ------------------------------------------------------------------------------------------------------------------------------- !
! [Project Euler] 0002                                                                                                            !
! https://projecteuler.net/problem=2                                                                                              !
! http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%202                                                        !
! ------------------------------------------------------------------------------------------------------------------------------- !
program main

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: Problem0002

  ! Require all variables to be explicitly declared
  implicit none

  ! variables for this <program>
  integer( kind=INT64 ) :: itr, limit

  do itr = 1,15, 1
    limit = 4_INT64 * 10**itr
    write( unit=output_unit, fmt='(I3,2(I20))', advance='yes' ) itr, limit, Problem0002_01( limit )
  end do
  read *
  
end program main
! ------------------------------------------------------------------------------------------------------------------------------- !
! gfortran ^                                                                                                                      !
! -c ^                                                                                                                            !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! D:\GitHub\Fortran\ProjectEuler\Problem0002\Problem0002_01.f08 ^                                                                 !
! D:\GitHub\Fortran\ProjectEuler\Problem0002\main.f08                                                                             !
!                                                                                                                                 !
! gfortran ^                                                                                                                      !
! -o Problem0002_01.exe ^                                                                                                         !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! D:\gfortran\Problem0002_01.o ^                                                                                                  !
! D:\gfortran\main.o                                                                                                              !
! ------------------------------------------------------------------------------------------------------------------------------- !