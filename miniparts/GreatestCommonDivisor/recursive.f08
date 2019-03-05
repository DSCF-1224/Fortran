! -------------------------------------------------------------------------------------------------------------------------------- !
! [target]
! calculate the greatest common divisor of integer `n1` and `n2`
! -------------------------------------------------------------------------------------------------------------------------------- !

!  input : INT8 and INT8
! output : INT8
recursive pure function GCD_RCR_INT08 (n1, n2) result (gcd)

  ! argument of this <function>
  integer (kind=INT8), intent (in) :: n1, n2

  ! return value of this <function>
  integer (kind=INT8) :: gcd

  ! STEP.01 !
  if (n2 .eq. 0_INT8) then
  gcd = n1
  else
  gcd = GCD_RCR_INT08 (n1= n2, n2=mod (n1, n2))
  end if

  ! STEP.END !
  return

end function GCD_RCR_INT08


!  input : INT16 and INT16
! output : INT16
recursive pure function GCD_RCR_INT16 (n1, n2) result (gcd)

  ! argument of this <function>
  integer (kind=INT16), intent (in) :: n1, n2

  ! return value of this <function>
  integer (kind=INT16) :: gcd

  ! STEP.01 !
  if (n2 .eq. 0_INT16) then
  gcd = n1
  else
  gcd = GCD_RCR_INT16 (n1= n2, n2=mod (n1, n2))
  end if

  ! STEP.END !
  return

end function GCD_RCR_INT16


!  input : INT32 and INT32
! output : INT32
recursive pure function GCD_RCR_INT32 (n1, n2) result (gcd)

  ! argument of this <function>
  integer (kind=INT32), intent (in) :: n1, n2

  ! return value of this <function>
  integer (kind=INT32) :: gcd

  ! STEP.01 !
  if (n2 .eq. 0_INT32) then
  gcd = n1
  else
  gcd = GCD_RCR_INT32 (n1= n2, n2=mod (n1, n2))
  end if

  ! STEP.END !
  return

end function GCD_RCR_INT32


!  input : INT64 and INT64
! output : INT64
recursive pure function GCD_RCR_INT64 (n1, n2) result (gcd)

  ! argument of this <function>
  integer (kind=INT64), intent (in) :: n1, n2

  ! return value of this <function>
  integer (kind=INT64) :: gcd

  ! STEP.01 !
  if (n2 .eq. 0_INT64) then
  gcd = n1
  else
  gcd = GCD_RCR_INT64 (n1= n2, n2=mod (n1, n2))
  end if

  ! STEP.END !
  return

end function GCD_RCR_INT64

! -------------------------------------------------------------------------------------------------------------------------------- !
! End of Source Code                                                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !
