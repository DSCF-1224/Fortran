! -------------------------------------------------------------------------------------------------------------------------------- !
! [target]                                                                                                                         !
! judge whether the loaded integer is a prime number                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !

pure function IsPrime_INT8 (target) result(stat)

  ! arguments for this <function>
  integer(kind=INT8), intent(in) :: target

  ! return value of this <function>
  logical :: stat

  ! variables for this <function>
  integer(kind=INT8) :: fctr_itr, fctr_max

  ! STEP.01
  ! if the integer `target` was less than or equal to `1`
  ! => return `.false.`
  if (target .le. 1_INT8) then

    stat = .false.; return
  
  ! STEP.02
  ! if the integer `target` was less than `4`
  ! <=> if the integer `target` was equal to `2` or `3`
  !  => return `.true.`
  else if (target .lt. 4_INT8) then
  
    stat = .true.; return
  
  ! STEP.03
  ! if the integer `target` was even number
  ! => return `.false.`
  else if (IsEven(target)) then
  
    stat = .false.; return
  
  ! STEP.04
  ! if the integer `target` was less than `9`
  ! <=> if the `target` was equal to `4` or `6` or `8`
  !  => return `.true.`
  else if (target .lt. 9_INT8) then
  
    stat = .true.; return
  
  ! STEP.05
  ! if the integer `target` was the multipe of `3`
  ! => return `false`
  else if (IsMultiple(target=target, ref=3_INT8)) then
  
    stat = .false.; return
  
  ! STEP.06
  else
  
    ! STEP.06.01
    ! calculate the upper limit of iteration
    fctr_max = int( sqrt( real(target, kind=REAL32) ), kind=INT8 )
    
    ! STEP.06.02
    ! judge whether `target` is the prime number repeatedly
    do fctr_itr = 5_INT8, fctr_max, 6_INT8
    
      if ( IsMultiple(target=target, ref=fctr_itr) .or. IsMultiple(target=target, ref=fctr_itr+2_INT8) ) then
      stat = .false.; return
      end if
    
    end do
    
    ! STEP.06.03
    stat = .true.
    return
  
  end if

end function IsPrime_INT8


pure function IsPrime_INT16 (target) result(stat)

  ! arguments for this <function>
  integer(kind=INT16), intent(in) :: target

  ! return value of this <function>
  logical :: stat

  ! variables for this <function>
  integer(kind=INT16) :: fctr_itr, fctr_max

  ! STEP.01
  ! if the integer `target` was less than or equal to `1`
  ! => return `.false.`
  if (target .le. 1_INT16) then

    stat = .false.; return
  
  ! STEP.02
  ! if the integer `target` was less than `4`
  ! <=> if the integer `target` was equal to `2` or `3`
  !  => return `.true.`
  else if (target .lt. 4_INT16) then
  
    stat = .true.; return
  
  ! STEP.03
  ! if the integer `target` was even number
  ! => return `.false.`
  else if (IsEven(target)) then
  
    stat = .false.; return
  
  ! STEP.04
  ! if the integer `target` was less than `9`
  ! <=> if the `target` was equal to `4` or `6` or `8`
  !  => return `.true.`
  else if (target .lt. 9_INT16) then
  
    stat = .true.; return
  
  ! STEP.05
  ! if the integer `target` was the multipe of `3`
  ! => return `false`
  else if (IsMultiple(target=target, ref=3_INT16)) then
  
    stat = .false.; return
  
  ! STEP.06
  else
  
    ! STEP.06.01
    ! calculate the upper limit of iteration
    fctr_max = int( sqrt( real(target, kind=REAL32) ), kind=INT16 )
    
    ! STEP.06.02
    ! judge whether `target` is the prime number repeatedly
    do fctr_itr = 5_INT16, fctr_max, 6_INT16
    
      if ( IsMultiple(target=target, ref=fctr_itr) .or. IsMultiple(target=target, ref=fctr_itr+2_INT16) ) then
      stat = .false.; return
      end if
    
    end do
    
    ! STEP.06.03
    stat = .true.
    return
  
  end if

end function IsPrime_INT16


pure function IsPrime_INT32 (target) result(stat)

  ! arguments for this <function>
  integer(kind=INT32), intent(in) :: target

  ! return value of this <function>
  logical :: stat

  ! variables for this <function>
  integer(kind=INT32) :: fctr_itr, fctr_max

  ! STEP.01
  ! if the integer `target` was less than or equal to `1`
  ! => return `.false.`
  if (target .le. 1_INT32) then

    stat = .false.; return
  
  ! STEP.02
  ! if the integer `target` was less than `4`
  ! <=> if the integer `target` was equal to `2` or `3`
  !  => return `.true.`
  else if (target .lt. 4_INT32) then
  
    stat = .true.; return
  
  ! STEP.03
  ! if the integer `target` was even number
  ! => return `.false.`
  else if (IsEven(target)) then
  
    stat = .false.; return
  
  ! STEP.04
  ! if the integer `target` was less than `9`
  ! <=> if the `target` was equal to `4` or `6` or `8`
  !  => return `.true.`
  else if (target .lt. 9_INT32) then
  
    stat = .true.; return
  
  ! STEP.05
  ! if the integer `target` was the multipe of `3`
  ! => return `false`
  else if (IsMultiple(target=target, ref=3_INT32)) then
  
    stat = .false.; return
  
  ! STEP.06
  else
  
    ! STEP.06.01
    ! calculate the upper limit of iteration
    fctr_max = int( sqrt( real(target, kind=REAL32) ), kind=INT32 )
    
    ! STEP.06.02
    ! judge whether `target` is the prime number repeatedly
    do fctr_itr = 5_INT32, fctr_max, 6_INT32
    
      if ( IsMultiple(target=target, ref=fctr_itr) .or. IsMultiple(target=target, ref=fctr_itr+2_INT32) ) then
      stat = .false.; return
      end if
    
    end do
    
    ! STEP.06.03
    stat = .true.
    return
  
  end if

end function IsPrime_INT32


pure function IsPrime_INT64 (target) result(stat)

  ! arguments for this <function>
  integer(kind=INT64), intent(in) :: target

  ! return value of this <function>
  logical :: stat

  ! variables for this <function>
  integer(kind=INT64) :: fctr_itr, fctr_max

  ! STEP.01
  ! if the integer `target` was less than or equal to `1`
  ! => return `.false.`
  if (target .le. 1_INT64) then

    stat = .false.; return
  
  ! STEP.02
  ! if the integer `target` was less than `4`
  ! <=> if the integer `target` was equal to `2` or `3`
  !  => return `.true.`
  else if (target .lt. 4_INT64) then
  
    stat = .true.; return
  
  ! STEP.03
  ! if the integer `target` was even number
  ! => return `.false.`
  else if (IsEven(target)) then
  
    stat = .false.; return
  
  ! STEP.04
  ! if the integer `target` was less than `9`
  ! <=> if the `target` was equal to `4` or `6` or `8`
  !  => return `.true.`
  else if (target .lt. 9_INT64) then
  
    stat = .true.; return
  
  ! STEP.05
  ! if the integer `target` was the multipe of `3`
  ! => return `false`
  else if (IsMultiple(target=target, ref=3_INT64)) then
  
    stat = .false.; return
  
  ! STEP.06
  else
  
    ! STEP.06.01
    ! calculate the upper limit of iteration
    fctr_max = int( sqrt( real(target, kind=REAL32) ), kind=INT64 )
    
    ! STEP.06.02
    ! judge whether `target` is the prime number repeatedly
    do fctr_itr = 5_INT64, fctr_max, 6_INT64
    
      if ( IsMultiple(target=target, ref=fctr_itr) .or. IsMultiple(target=target, ref=fctr_itr+2_INT64) ) then
      stat = .false.; return
      end if
    
    end do
    
    ! STEP.06.03
    stat = .true.
    return
  
  end if

end function IsPrime_INT64

! -------------------------------------------------------------------------------------------------------------------------------- !
! End of Source Code                                                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !
