!----------------------------------------------------------------------------------------------------------------------------------!
! [target]                                                                                                                         !
! to use DATE_AND_TIME                                                                                                             !
!----------------------------------------------------------------------------------------------------------------------------------!
module support_date_and_time

  ! <module>s to import
  use, intrinsic :: iso_fortran_env


  ! require all variables to be explicitly declared
  implicit none


  ! accessibility of <subroutine>s, <function>s and <type>s in this <module>
  public :: Type_Date_And_Time
  public :: DATE_AND_TIME_UsingType


  ! <interface> for this <module>
  interface DATE_AND_TIME
    module procedure :: DATE_AND_TIME_UsingType
  end interface


  ! <type>s for this <module>
  type Type_Date_And_Time
    character( len= 8, kind=1 ) :: date
    character( len=10, kind=1 ) :: time
    character( len= 5, kind=1 ) :: zone
    integer( kind= INT32 )      :: values(1:8)
  end type Type_Date_And_Time


  ! <subroutine>s and <function>s for this <module>
  contains


  ! call <SYSTEM_CLOCK> using a structure which is define here
  subroutine DATE_AND_TIME_UsingType( struct )

    ! argument for this <subroutine>
    type(Type_Date_And_Time), intent(inout) :: struct

    ! call <SYSTEM_CLOCK>
    call DATE_AND_TIME( &!
      date   = struct % date      ,&!
      time   = struct % time      ,&!
      zone   = struct % zone      ,&!
      values = struct % values(:)  &!
    )

    ! return the parent process
    return

  end subroutine DATE_AND_TIME_UsingType


end module support_date_and_time