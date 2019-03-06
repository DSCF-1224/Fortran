! -------------------------------------------------------------------------------------------------------------------------------- !
! [target]                                                                                                                         !
! judge whether the loaded integer is an even number                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !

pure function IsEven_INT8(target) result(stat)

  ! arguments for this <function>
  integer (kind=INT8), intent(in) :: target

  ! return value of this <function>
  logical :: stat

  ! STEP.END
  stat = IsMultiple_INT8(target=target, ref=2_INT8)
  return

end function IsEven_INT8


pure function IsEven_INT16(target) result(stat)

  ! arguments for this <function>
  integer (kind=INT16), intent(in) :: target

  ! return value of this <function>
  logical :: stat

  ! STEP.END
  stat = IsMultiple_INT16(target=target, ref=2_INT16)
  return

end function IsEven_INT16


pure function IsEven_INT32(target) result(stat)

  ! arguments for this <function>
  integer (kind=INT32), intent(in) :: target

  ! return value of this <function>
  logical :: stat

  ! STEP.END
  stat = IsMultiple_INT32(target=target, ref=2_INT32)
  return

end function IsEven_INT32


pure function IsEven_INT64(target) result(stat)

  ! arguments for this <function>
  integer (kind=INT64), intent(in) :: target

  ! return value of this <function>
  logical :: stat

  ! STEP.END
  stat = IsMultiple_INT64(target=target, ref=2_INT64)
  return

end function IsEven_INT64

! -------------------------------------------------------------------------------------------------------------------------------- !
! End of Source Code                                                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !
