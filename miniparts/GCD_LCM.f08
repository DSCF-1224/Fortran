! code created : 2019.01.20
! code updated : 2019.01.20
!
! compiler for test : GNU Fortran 8.1.0
!
! [reference]
! https://gihyo.jp/book/2018/978-4-7741-9690-9
! pp.82-83

module GCD_LCM

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! accessibility of <subroutine>s and <function>s in this <module>
  public  :: GCD_RCR
  public  :: LCM_RCR
  private :: GCD_RCR_INT8, GCD_RCR_INT16, GCD_RCR_INT32, GCD_RCR_INT64
  private :: LCM_RCR_INT8, LCM_RCR_INT16, LCM_RCR_INT32, LCM_RCR_INT64

  ! <interface>s for this <module>
  interface GCD_RCR
      module procedure GCD_RCR_INT8
      module procedure GCD_RCR_INT16
      module procedure GCD_RCR_INT32
      module procedure GCD_RCR_INT64
  end interface ! GCD_RCR

  interface LCM_RCR
      module procedure LCM_RCR_INT8
      module procedure LCM_RCR_INT16
      module procedure LCM_RCR_INT32
      module procedure LCM_RCR_INT64
  end interface ! LCM_RCR

  ! <subroutine>s and <function>s in this <module> is below
  contains

  ! greatest common divsor (recursive) !
  recursive pure function GCD_RCR_INT8( num1, num2 ) result( retval )

    ! argument of this <function>
    integer( kind=INT8 ), intent(in) :: num1, num2

    ! return value of this <function>
    integer( kind=INT8 ) :: retval

    ! STEP.01 !
    if( num2 .eq. 0_INT8 ) then
      retval = num1
    else
      retval = GCD_RCR_INT8( num1= num2, num2= mod( num1, num2 ) )
    end if

    ! STEP.END !
    return

  end function GCD_RCR_INT8

  recursive pure function GCD_RCR_INT16( num1, num2 ) result( retval )

    ! argument of this <function>
    integer( kind=INT16 ), intent(in) :: num1, num2

    ! return value of this <function>
    integer( kind=INT16 ) :: retval

    ! STEP.01 !
    if( num2 .eq. 0_INT16 ) then
      retval = num1
    else
      retval = GCD_RCR_INT16( num1= num2, num2= mod( num1, num2 ) )
    end if

    ! STEP.END !
    return

  end function GCD_RCR_INT16

  recursive pure function GCD_RCR_INT32( num1, num2 ) result( retval )

    ! argument of this <function>
    integer( kind=INT32 ), intent(in) :: num1, num2

    ! return value of this <function>
    integer( kind=INT32 ) :: retval

    ! STEP.01 !
    if( num2 .eq. 0_INT32 ) then
      retval = num1
    else
      retval = GCD_RCR_INT32( num1= num2, num2= mod( num1, num2 ) )
    end if

    ! STEP.END !
    return

  end function GCD_RCR_INT32

  recursive pure function GCD_RCR_INT64( num1, num2 ) result( retval )

    ! argument of this <function>
    integer( kind=INT64 ), intent(in) :: num1, num2

    ! return value of this <function>
    integer( kind=INT64 ) :: retval

    ! STEP.01 !
    if( num2 .eq. 0_INT64 ) then
      retval = num1
    else
      retval = GCD_RCR_INT64( num1= num2, num2= mod( num1, num2 ) )
    end if

    ! STEP.END !
    return

  end function GCD_RCR_INT64

  ! least common multiple (recursive) !
  pure function LCM_RCR_INT8( num1, num2 ) result( retval )

    ! argument of this <function>
    integer( kind=INT8 ), intent(in) :: num1, num2

    ! return value of this <function>
    integer( kind=INT8 ) :: retval

    retval = abs( num1*num2 ) / GCD_RCR_INT8( num1, num2 )
    return

  end function LCM_RCR_INT8

  pure function LCM_RCR_INT16( num1, num2 ) result( retval )

    ! argument of this <function>
    integer( kind=INT16 ), intent(in) :: num1, num2

    ! return value of this <function>
    integer( kind=INT16 ) :: retval

    retval = abs( num1*num2 ) / GCD_RCR_INT16( num1, num2 )
    return

  end function LCM_RCR_INT16

  pure function LCM_RCR_INT32( num1, num2 ) result( retval )

    ! argument of this <function>
    integer( kind=INT32 ), intent(in) :: num1, num2

    ! return value of this <function>
    integer( kind=INT32 ) :: retval

    retval = abs( num1*num2 ) / GCD_RCR_INT32( num1, num2 )
    return

  end function LCM_RCR_INT32

  pure function LCM_RCR_INT64( num1, num2 ) result( retval )

    ! argument of this <function>
    integer( kind=INT64 ), intent(in) :: num1, num2

    ! return value of this <function>
    integer( kind=INT64 ) :: retval

    retval = abs( num1*num2 ) / GCD_RCR_INT64( num1, num2 )
    return

  end function LCM_RCR_INT64

end module