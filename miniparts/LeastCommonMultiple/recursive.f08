! -------------------------------------------------------------------------------------------------------------------------------- !
! [target]
! calculate the least common multiple of integer `n1` and `n2`
! -------------------------------------------------------------------------------------------------------------------------------- !

!  input : INT8 and INT8
! output : INT8
pure function LCM_RCR_INT08 (n1, n2) result (lcm)

  ! argument of this <function>
  integer (kind=INT8), intent(in) :: n1, n2

  ! return value of this <function>
  integer (kind=INT8) :: lcm

  lcm = abs (n1*n2) / GCD_RCR_INT08 (n1, n2)
  return

end function LCM_RCR_INT08


!  input : INT16 and INT16
! output : INT16
pure function LCM_RCR_INT16 (n1, n2) result (lcm)

  ! argument of this <function>
  integer (kind=INT16), intent(in) :: n1, n2

  ! return value of this <function>
  integer (kind=INT16) :: lcm

  lcm = abs (n1*n2) / GCD_RCR_INT16 (n1, n2)
  return

end function LCM_RCR_INT16


!  input : INT32 and INT32
! output : INT32
pure function LCM_RCR_INT32 (n1, n2) result (lcm)

  ! argument of this <function>
  integer (kind=INT32), intent(in) :: n1, n2

  ! return value of this <function>
  integer (kind=INT32) :: lcm

  lcm = abs (n1*n2) / GCD_RCR_INT32 (n1, n2)
  return

end function LCM_RCR_INT32


!  input : INT64 and INT64
! output : INT64
pure function LCM_RCR_INT64 (n1, n2) result (lcm)

  ! argument of this <function>
  integer (kind=INT64), intent(in) :: n1, n2

  ! return value of this <function>
  integer (kind=INT64) :: lcm

  lcm = abs (n1*n2) / GCD_RCR_INT64 (n1, n2)
  return

end function LCM_RCR_INT64

! -------------------------------------------------------------------------------------------------------------------------------- !
! End of Source Code                                                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !
