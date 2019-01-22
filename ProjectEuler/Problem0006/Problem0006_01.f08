! ------------------------------------------------------------------------------------------------------------------------------- !
! [Project Euler] 0006                                                                                                            !
! https://projecteuler.net/problem=6                                                                                              !
! http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%206                                                        !
! ------------------------------------------------------------------------------------------------------------------------------- !
module Problem0006

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of <subroutine>s and <function>s in this <module>
  public  :: Problem0006_01       ! interface
  private :: Problem0006_01_INT32 ! function
  private :: Problem0006_01_INT64 ! function
  private :: Problem0006_02_INT32 ! function
  private :: Problem0006_02_INT64 ! function

  ! <interface>s for this <program>
  interface Problem0006_01
    module procedure Problem0006_01_INT32
    module procedure Problem0006_01_INT64
  end interface
  
  interface Problem0006_02
    module procedure Problem0006_02_INT32
    module procedure Problem0006_02_INT64
  end interface

  ! <subroutine>s and <function>s in this <module> is below
  contains

  pure function Problem0006_01_INT32( target ) result( diff )

    ! arguments for this <function>
    integer( kind= INT32 ), intent(in) :: target

    ! return value of this <function>
    integer( kind= INT32 ) :: diff

    ! variables for this <function>
    integer( kind= INT32 ) :: sum_squared, squared_sum
    integer( kind= INT32 ) :: itr

    ! STEP.01 !
    sum_squared = 0_INT32
    squared_sum = 0_INT32

    ! STEP.02 !
    do itr = 1, target, 1
      sum_squared = sum_squared + itr*itr
      squared_sum = squared_sum + itr
    end do

    ! STEP.03 !
    squared_sum = squared_sum * squared_sum

    ! STEP.04 !
    diff = squared_sum - sum_squared
    return

  end function Problem0006_01_INT32

  pure function Problem0006_01_INT64( target ) result( diff )

    ! arguments for this <function>
    integer( kind= INT64 ), intent(in) :: target

    ! return value of this <function>
    integer( kind= INT64 ) :: diff

    ! variables for this <function>
    integer( kind= INT64 ) :: sum_squared, squared_sum
    integer( kind= INT64 ) :: itr

    ! STEP.01 !
    sum_squared = 0_INT64
    squared_sum = 0_INT64

    ! STEP.02 !
    do itr = 1, target, 1
      sum_squared = sum_squared + itr*itr
      squared_sum = squared_sum + itr
    end do

    ! STEP.03 !
    squared_sum = squared_sum * squared_sum

    ! STEP.04 !
    diff = squared_sum - sum_squared
    return

  end function Problem0006_01_INT64


  pure function Problem0006_02_INT32( target ) result( diff )

    ! arguments for this <function>
    integer( kind= INT32 ), intent(in) :: target

    ! return value of this <function>
    integer( kind= INT32 ) :: diff

    ! variables for this <function>
    integer( kind= INT32 ) :: sum_squared, squared_sum

    ! STEP.01 !
    sum_squared = target * ( target + 1_INT32 ) * ( 2_INT32 * target + 1_INT32 ) / 6_INT32

    ! STEP.02 !
    squared_sum = target * ( target + 1_INT32 ) / 2_INT32
    squared_sum = squared_sum * squared_sum

    ! STEP.03 !
    diff = squared_sum - sum_squared
    return

  end function Problem0006_02_INT32

  pure function Problem0006_02_INT64( target ) result( diff )

    ! arguments for this <function>
    integer( kind= INT64 ), intent(in) :: target

    ! return value of this <function>
    integer( kind= INT64 ) :: diff

    ! variables for this <function>
    integer( kind= INT64 ) :: sum_squared, squared_sum

    ! STEP.01 !
    sum_squared = target * ( target + 1_INT64 ) * ( 2_INT64 * target + 1_INT64 ) / 6_INT64

    ! STEP.02 !
    squared_sum = target * ( target + 1_INT64 ) / 2_INT64
    squared_sum = squared_sum * squared_sum

    ! STEP.03 !
    diff = squared_sum - sum_squared
    return

  end function Problem0006_02_INT64

end module Problem0006
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