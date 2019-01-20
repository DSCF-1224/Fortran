! ------------------------------------------------------------------------------------------------------------------------------- !
! [Project Euler] 0002                                                                                                            !
! https://projecteuler.net/problem=2                                                                                              !
! http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%202                                                        !
! ------------------------------------------------------------------------------------------------------------------------------- !
program main

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: Problem0002
  use, non_intrinsic :: support_system_clock

  ! Require all variables to be explicitly declared
  implicit none

  ! variables for this <program>
  integer( kind=INT64 )     :: itr, limit, sum
  type( Type_System_Clock ) :: System_Clock_Start, System_Clock_End

  itr   = 6_INT64
  limit = 4_INT64 * 10_INT64 ** itr

  call SYSTEM_CLOCK_UsingType( System_Clock_Start )
  sum = Problem0002_01( limit )
  call SYSTEM_CLOCK_UsingType( System_Clock_End )

  write( unit=output_unit, fmt='(I3,2(I20))',   advance='yes' ) itr, limit, sum
  write( unit=output_unit, fmt='(A,ES23.15e3)', advance='yes' ) &!
    '[CPU TIME]', &!
    SYSTEM_CLOCK_ElapsedTime( start= System_Clock_Start, stop= System_Clock_End )

  call SYSTEM_CLOCK_UsingType( System_Clock_Start )
  sum = Problem0002_02( limit )
  call SYSTEM_CLOCK_UsingType( System_Clock_End )

  write( unit=output_unit, fmt='(I3,2(I20))',   advance='yes' ) itr, limit, sum
  write( unit=output_unit, fmt='(A,ES23.15e3)', advance='yes' ) &!
    '[CPU TIME]', &!
    SYSTEM_CLOCK_ElapsedTime( start= System_Clock_Start, stop= System_Clock_End )

  read *
  
end program main
! ------------------------------------------------------------------------------------------------------------------------------- !
! gfortran ^                                                                                                                      !
! -c ^                                                                                                                            !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! D:\GitHub\Fortran\support\support_system_clock.f08 ^                                                                            !
! D:\GitHub\Fortran\ProjectEuler\Problem0002\Problem0002_01.f08 ^                                                                 !
! D:\GitHub\Fortran\ProjectEuler\Problem0002\main.f08                                                                             !
!                                                                                                                                 !
! gfortran ^                                                                                                                      !
! -o Problem0002_01.exe ^                                                                                                         !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! support_system_clock.o ^                                                                                                        !
! D:\gfortran\Problem0002_01.o ^                                                                                                  !
! D:\gfortran\main.o                                                                                                              !
! ------------------------------------------------------------------------------------------------------------------------------- !