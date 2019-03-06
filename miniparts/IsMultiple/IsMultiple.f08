! -------------------------------------------------------------------------------------------------------------------------------- !
! [target]                                                                                                                         !
! judge whether the loaded integer is a multiple of the another loaded integer                                                     !
! -------------------------------------------------------------------------------------------------------------------------------- !

pure function IsMultiple_INT8(target, ref) result(stat)

  ! arguments for this <function>
  integer (kind=INT8), intent(in) :: target, ref

  ! return value of this <function>
  logical :: stat

  ! STEP.END
  stat = mod(target, ref) .eq. 0_INT8
  return

end function IsMultiple_INT8


pure function IsMultiple_INT16(target, ref) result(stat)

  ! arguments for this <function>
  integer (kind=INT16), intent(in) :: target, ref

  ! return value of this <function>
  logical :: stat

  ! STEP.END
  stat = mod(target, ref) .eq. 0_INT16
  return

end function IsMultiple_INT16


pure function IsMultiple_INT32(target, ref) result(stat)

  ! arguments for this <function>
  integer (kind=INT32), intent(in) :: target, ref

  ! return value of this <function>
  logical :: stat

  ! STEP.END
  stat = mod(target, ref) .eq. 0_INT32
  return

end function IsMultiple_INT32


pure function IsMultiple_INT64(target, ref) result(stat)

  ! arguments for this <function>
  integer (kind=INT64), intent(in) :: target, ref

  ! return value of this <function>
  logical :: stat

  ! STEP.END
  stat = mod(target, ref) .eq. 0_INT64
  return

end function IsMultiple_INT64

! -------------------------------------------------------------------------------------------------------------------------------- !
! End of Source Code                                                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !
