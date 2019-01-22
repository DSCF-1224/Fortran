! ------------------------------------------------------------------------------------------------------------------------------- !
! [Project Euler] 0002                                                                                                            !
! https://projecteuler.net/problem=2                                                                                              !
! http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%202                                                        !
! ------------------------------------------------------------------------------------------------------------------------------- !
module Problem0002

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of <subroutine>s and <function>s in this <module>
  public  :: Problem0002_01
  public  :: Problem0002_02
  private :: Calc_FibonacciNum

  ! constants for this <module>
  integer( kind=INT64 ), private, parameter :: Fibonacci_1st = 1_INT64
  integer( kind=INT64 ), private, parameter :: Fibonacci_2nd = 2_INT64
  
  ! <subroutine>s and <function>s in this <module> is below
  contains

  recursive pure function Calc_FibonacciNum( term ) result( retval )

    ! argument of this <function>
    integer( kind=INT32 ), intent(in) :: term

    ! return value of this <function>
    integer( kind=INT64 ) :: retval

    select case (term)
      case( 1_INT32 ); retval = Fibonacci_1st
      case( 2_INT32 ); retval = Fibonacci_2nd
      case default;    retval = Calc_FibonacciNum( term-1_INT32 ) + Calc_FibonacciNum( term-2_INT32 )
    end select

    return

  end function Calc_FibonacciNum

  pure function Problem0002_01( limit ) result( sum )

    ! argument of this <function>
    integer( kind=INT64 ), intent(in) :: limit

    ! return value of this <function>
    integer( kind=INT64 ) :: sum

    ! support variables for this <function>
    integer( kind=INT32 ) :: itr
    integer( kind=INT64 ) :: buf_Fibonacci


    ! STEP.01 !
    itr           = 1_INT32
    buf_Fibonacci = Calc_FibonacciNum( itr )
    sum           = 0_INT64

    ! STEP.02 !
    do while( buf_Fibonacci .lt. limit )
      if( mod( buf_Fibonacci, 2_INT64 ) .eq. 0_INT64  ) sum = sum + buf_Fibonacci
      itr           = itr + 1_INT32
      buf_Fibonacci = Calc_FibonacciNum( itr )
    end do

    ! STEP.END !
    return

  end function Problem0002_01

  pure function Problem0002_02( limit ) result( sum )

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

  end function Problem0002_02

end module Problem0002
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