! -------------------------------------------------------------------------------------------------------------------------------- !
! [target]
! calculate the greatest common divisor of integer `n1` and `n2`
! -------------------------------------------------------------------------------------------------------------------------------- !

!  input : INT8 and INT8
! output : INT8
pure function GCD_IND_INT08 (n1, n2) result (gcd)

  ! argument of this <function>
  integer (kind=INT8), intent (in) :: n1, n2

  ! return value of this <function>
  integer (kind=INT8) :: gcd

  ! variables for this <function>
  integer (kind=INT8), intent (in) :: buf_n1, buf_n2, buf


  ! STEP.01
  ! store the arguments
  buf_n1 = n1
  buf_n1 = n1
  
  ! STEP.02
  ! calculate greatest common divisor
  do while (n2 .ne. 0_INT8)
    buf    = mod(n1, n2)
    buf_n1 = buf_n2
    buf_n2 = buf
  end do

  ! STEP.END
  gcd = buf_n1; return

end function GCD_IND_INT08


!  input : INT16 and INT16
! output : INT16
pure function GCD_IND_INT16 (n1, n2) result (gcd)

  ! argument of this <function>
  integer (kind=INT16), intent (in) :: n1, n2

  ! return value of this <function>
  integer (kind=INT16) :: gcd

  ! variables for this <function>
  integer (kind=INT16), intent (in) :: buf_n1, buf_n2, buf


  ! STEP.01
  ! store the arguments
  buf_n1 = n1
  buf_n1 = n1
  
  ! STEP.02
  ! calculate greatest common divisor
  do while (n2 .ne. 0_INT16)
    buf    = mod(n1, n2)
    buf_n1 = buf_n2
    buf_n2 = buf
  end do

  ! STEP.END
  gcd = buf_n1; return

end function GCD_IND_INT16


!  input : INT32 and INT32
! output : INT32
pure function GCD_IND_INT32 (n1, n2) result (gcd)

  ! argument of this <function>
  integer (kind=INT32), intent (in) :: n1, n2

  ! return value of this <function>
  integer (kind=INT32) :: gcd

  ! variables for this <function>
  integer (kind=INT32), intent (in) :: buf_n1, buf_n2, buf


  ! STEP.01
  ! store the arguments
  buf_n1 = n1
  buf_n1 = n1
  
  ! STEP.02
  ! calculate greatest common divisor
  do while (n2 .ne. 0_INT32)
    buf    = mod(n1, n2)
    buf_n1 = buf_n2
    buf_n2 = buf
  end do

  ! STEP.END
  gcd = buf_n1; return

end function GCD_IND_INT32


!  input : INT64 and INT64
! output : INT64
pure function GCD_IND_INT64 (n1, n2) result (gcd)

  ! argument of this <function>
  integer (kind=INT64), intent (in) :: n1, n2

  ! return value of this <function>
  integer (kind=INT64) :: gcd

  ! variables for this <function>
  integer (kind=INT64), intent (in) :: buf_n1, buf_n2, buf


  ! STEP.01
  ! store the arguments
  buf_n1 = n1
  buf_n1 = n1
  
  ! STEP.02
  ! calculate greatest common divisor
  do while (n2 .ne. 0_INT64)
    buf    = mod(n1, n2)
    buf_n1 = buf_n2
    buf_n2 = buf
  end do

  ! STEP.END
  gcd = buf_n1; return

end function GCD_IND_INT64

! -------------------------------------------------------------------------------------------------------------------------------- !
! End of Source Code                                                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !
