! ------------------------------------------------------------------------------------------------------------------------------- !
! [Project Euler] 0003                                                                                                            !
! https://projecteuler.net/problem=3                                                                                              !
! http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%203                                                        !
! ------------------------------------------------------------------------------------------------------------------------------- !
program main

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: Problem0003

  ! Require all variables to be explicitly declared
  implicit none

  ! variables for this <program>
  integer( kind=INT64 ) :: target

  target = 600851475143_INT64

  write( unit=output_unit, fmt='(4(I14))', advance='yes' ) &!
    target,                   &!
    Problem0003_01( target ), &!
    Problem0003_02( target ), &!
    Problem0003_03( target )

  read *
  
end program main
! ------------------------------------------------------------------------------------------------------------------------------- !
! gfortran ^                                                                                                                      !
! -c ^                                                                                                                            !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! D:\GitHub\Fortran\ProjectEuler\Problem0003\Problem0003_01.f08 ^                                                                 !
! D:\GitHub\Fortran\ProjectEuler\Problem0003\main.f08                                                                             !
!                                                                                                                                 !
! gfortran ^                                                                                                                      !
! -o Problem0003_01.exe ^                                                                                                         !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! D:\gfortran\Problem0003_01.o ^                                                                                                  !
! D:\gfortran\main.o                                                                                                              !
! ------------------------------------------------------------------------------------------------------------------------------- !