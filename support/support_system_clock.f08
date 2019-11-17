!----------------------------------------------------------------------------------------------------------------------------------!
! [target]                                                                                                                         !
! support to use SYSTEM_CLOCK                                                                                                      !
!----------------------------------------------------------------------------------------------------------------------------------!

module moduleSupportSystemClock

  ! <module>s to import
  use, intrinsic :: iso_fortran_env
  
   ! require all variables to be explicitly declared
  implicit none

  ! accessibility of the <subroutine>s and <function>s in this <module>
  public  :: typeSysClock
  private :: getDataSystemClock ! subroutine
  public  :: calcElapsedTime    ! function

  ! <type>s for this <module>
  type typeSysClock
    integer(INT64), private :: count
    integer(INT64), private :: count_rate
    integer(INT64), private :: count_max
  end type typeSysClock

  ! <interface>s for this <module>
  interface system_clock
    module procedure :: getDataSystemClock
  end interface system_clock

  ! contained <subroutine>s and <function>s are below
  contains

  subroutine getDataSystemClock ( objToStoreData )

    ! arguments for this <subroutine>
    type(typeSysClock), intent(out) :: objToStoreData

    ! STEP.01/01
    ! call intrinsic subroutine <SYSTEM_CLOCK>
    call system_clock(                        &!
      count      = objToStoreData%count,      &!
      count_rate = objToStoreData%count_rate, &!
      count_max  = objToStoreData%count_max   &!
    )

    ! STEP.END
    return

  end subroutine getDataSystemClock

  pure function calcElapsedTime ( objClockBegin, objClockEnd ) result( valElapsedTime )

    ! arguments for this <function>
    type(typeSysClock), intent(in) :: objClockBegin, objClockEnd

    ! return value of this <function>
    real(REAL64) :: valElapsedTime

    ! STEP.01/02
    ! calculate the `count` increment of the <SYSTEM_CLOCK>
    if ( objClockEnd%count .lt. objClockEnd%count ) then
      valElapsedTime = real( objClockEnd%count_max - objClockBegin%count + objClockEnd%count + 1_INT64, kind = REAL64 )
    else
      valElapsedTime = real( objClockEnd%count - objClockBegin%count, kind = REAL64 )
    end if

    ! STEP.02/02
    ! calculate the elapsed time
    valElapsedTime = valElapsedTime / objClockEnd%count_rate

    ! STEP.END
    return

  end function calcElapsedTime

end module moduleSupportSystemClock

!----------------------------------------------------------------------------------------------------------------------------------!
! End of the file                                                                                                                  !
!----------------------------------------------------------------------------------------------------------------------------------!
