!----------------------------------------------------------------------------------------------------------------------------------!
! [target]                                                                                                                         !
! to use SYSTEM_CLOCK                                                                                                              !
!----------------------------------------------------------------------------------------------------------------------------------!
module support_system_clock

  ! <module>s to import
  use, intrinsic :: iso_fortran_env


  ! require all variables to be explicitly declared
  implicit none


  ! accessibility of <subroutine>s, <function>s and <type>s in this <module>
  public :: Type_System_Clock
  public :: SYSTEM_CLOCK_ElapsedTime
  public :: SYSTEM_CLOCK_UsingType


  ! <interface> for this <module>
  interface SYSTEM_CLOCK
    module procedure :: SYSTEM_CLOCK_UsingType
  end interface


  ! <type>s for this <module>
  type Type_System_Clock
    integer( kind=INT64 ), public :: count
    integer( kind=INT64 ), public :: count_rate
    integer( kind=INT64 ), public :: count_max
  end type Type_System_Clock


  ! <subroutine>s and <function>s for this <module>
  contains


  ! call <SYSTEM_CLOCK> using a structure which is define here
  subroutine SYSTEM_CLOCK_UsingType( struct )

    ! argument for this <subroutine>
    type(Type_System_Clock), intent(inout) :: struct

    ! call <SYSTEM_CLOCK>
    call SYSTEM_CLOCK( struct%count, struct%count_rate, struct%count_max )

    ! return the parent process
    return

  end subroutine SYSTEM_CLOCK_UsingType


  ! calculate the spend time using <SYSTEM_CLOCK>
  ! [reference]
  ! http://www.nag-j.co.jp/fortran/tips/tips_PortableWayToTime.html#_PortableWayToTime
  pure function SYSTEM_CLOCK_ElapsedTime( start, stop ) result( elapsed_time )

    ! argument for this <function>
    type(Type_System_Clock), intent(in) :: start
    type(Type_System_Clock), intent(in) :: stop

    ! return value of this <function>
    real( kind=REAL64 ) :: elapsed_time

    if( stop%count .lt. start%count ) then
      elapsed_time = real( ( stop%count_max - start%count ) + stop%count + 1_INT64, kind= REAL64 ) / real( stop%count_rate )
      return
    else
      elapsed_time = real( stop%count - start%count, kind= REAL64 ) / real( stop%count_rate )
      return
    end if


  end function SYSTEM_CLOCK_ElapsedTime


end module support_system_clock