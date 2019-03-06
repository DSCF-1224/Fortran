! -------------------------------------------------------------------------------------------------------------------------------- !
! [target]                                                                                                                         !
! judge whether the loaded integer is an odd number                                                                                !
! -------------------------------------------------------------------------------------------------------------------------------- !

pure function IsOdd_INT8(target) result(stat)

  ! arguments for this <function>
  integer (kind=INT8), intent(in) :: target

  ! return value of this <function>
  logical :: stat

  ! STEP.END
  stat = .not. IsEven_INT8(target)
  return

end function IsOdd_INT8


pure function IsOdd_INT16(target) result(stat)

  ! arguments for this <function>
  integer (kind=INT16), intent(in) :: target

  ! return value of this <function>
  logical :: stat

  ! STEP.END
  stat = .not. IsEven_INT16(target)
  return

end function IsOdd_INT16


pure function IsOdd_INT32(target) result(stat)

  ! arguments for this <function>
  integer (kind=INT32), intent(in) :: target

  ! return value of this <function>
  logical :: stat

  ! STEP.END
  stat = .not. IsEven_INT32(target)
  return

end function IsOdd_INT32


pure function IsOdd_INT64(target) result(stat)

  ! arguments for this <function>
  integer (kind=INT64), intent(in) :: target

  ! return value of this <function>
  logical :: stat

  ! STEP.END
  stat = .not. IsEven_INT64(target)
  return

end function IsOdd_INT64

! -------------------------------------------------------------------------------------------------------------------------------- !
! End of Source Code                                                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !
