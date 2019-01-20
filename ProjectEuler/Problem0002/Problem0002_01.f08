! ------------------------------------------------------------------------------------------------------------------------------- !
! [Project Euler] 0002                                                                                                            !
! https://projecteuler.net/problem=2                                                                                              !
! http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%202                                                        !
! ------------------------------------------------------------------------------------------------------------------------------- !
module Problem0002

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! accessibility of <subroutine>s and <function>s in this <module>
  public :: Problem0002_01

  ! constants for this <module>
  integer( kind=INT64 ), private, parameter :: Fibonacci_1st = 1_INT64
  integer( kind=INT64 ), private, parameter :: Fibonacci_2nd = 2_INT64
  
  ! <subroutine>s and <function>s in this <module> is below
  contains

  pure function Problem0002_01( limit ) result( sum )

    ! argument of this <function>
    integer( kind=INT64 ), intent(in) :: limit

    ! return value of this <function>
    integer( kind=INT64 ) :: sum

    ! support variables for this <function>
    integer( kind=INT64 ) :: Fibonacci_buf1
    integer( kind=INT64 ) :: Fibonacci_buf2
    integer( kind=INT64 ) :: Fibonacci_buf3


    ! STEP.01 !
    Fibonacci_buf1 = Fibonacci_1st
    Fibonacci_buf2 = Fibonacci_2nd
    Fibonacci_buf3 = Fibonacci_1st + Fibonacci_2nd
    sum            = Fibonacci_2nd

    ! STEP.02 !
    do while( Fibonacci_buf3 .lt. limit )
      if( mod( Fibonacci_buf3, 2_INT64 ) .eq. 0_INT64  ) sum = sum + Fibonacci_buf3
      Fibonacci_buf1 = Fibonacci_buf2
      Fibonacci_buf2 = Fibonacci_buf3
      Fibonacci_buf3 = Fibonacci_buf1 + Fibonacci_buf2
    end do

    ! STEP.END !
    return

  end function Problem0002_01

end module Problem0002
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