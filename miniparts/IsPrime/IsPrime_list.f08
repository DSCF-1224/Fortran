! -------------------------------------------------------------------------------------------------------------------------------- !
! [target]                                                                                                                         !
! judge whether the loaded integer is a prime number                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !

pure function IsPrime_list_INT8 (target, list) result(stat)

  ! arguments for this <function>
  integer(kind=INT8), intent(in) :: target
  integer(kind=INT8), intent(in) :: list(:)

  ! return value of this <function>
  logical :: stat

  ! logical variables for this <function>
  integer(kind=INT8) :: itr_elem, len_list

  ! STEP.01
  ! if the integer `target` was less than `2`
  if (target .le. 1_INT8) then

    stat = .false.
    return

  ! STEP.02
  ! if the integer `target` was equal to `2` or `3`
  else if (target .le. 3_INT8) then

    stat = .true.
    return

  ! STEP.03
  ! if the integer `target` was bigger than `3`
  else

    ! get the length of the list of the prime numbers
    len_list = size(list, dim=1, kind=INT8)

    ! STEP.03
    ! judge whether the integer `target` is the multiple of `list(itr_elem)`
    do itr_elem = 1_INT8, len_list, 1_INT8

      if (IsMultiple(target=target, ref=list(itr_elem))) then
        ! STEP.BAD_END
        stat = .false.
        return
      end if

    end do

    ! STEP.TRUE_END
    stat = .true.
    return

  end if

end function IsPrime_list_INT8

pure function IsPrime_list_INT16 (target, list) result(stat)

  ! arguments for this <function>
  integer(kind=INT16), intent(in) :: target
  integer(kind=INT16), intent(in) :: list(:)

  ! return value of this <function>
  logical :: stat

  ! logical variables for this <function>
  integer(kind=INT16) :: itr_elem, len_list

  ! STEP.01
  ! if the integer `target` was less than `2`
  if (target .le. 1_INT16) then

    stat = .false.
    return

  ! STEP.02
  ! if the integer `target` was equal to `2` or `3`
  else if (target .le. 3_INT16) then

    stat = .true.
    return

  ! STEP.03
  ! if the integer `target` was bigger than `3`
  else

    ! get the length of the list of the prime numbers
    len_list = size(list, dim=1, kind=INT16)

    ! STEP.03
    ! judge whether the integer `target` is the multiple of `list(itr_elem)`
    do itr_elem = 1_INT16, len_list, 1_INT16

      if (IsMultiple(target=target, ref=list(itr_elem))) then
        ! STEP.BAD_END
        stat = .false.
        return
      end if

    end do

    ! STEP.TRUE_END
    stat = .true.
    return

  end if

end function IsPrime_list_INT16

pure function IsPrime_list_INT32 (target, list) result(stat)

  ! arguments for this <function>
  integer(kind=INT32), intent(in) :: target
  integer(kind=INT32), intent(in) :: list(:)

  ! return value of this <function>
  logical :: stat

  ! logical variables for this <function>
  integer(kind=INT32) :: itr_elem, len_list

  ! STEP.01
  ! if the integer `target` was less than `2`
  if (target .le. 1_INT32) then

    stat = .false.
    return

  ! STEP.02
  ! if the integer `target` was equal to `2` or `3`
  else if (target .le. 3_INT32) then

    stat = .true.
    return

  ! STEP.03
  ! if the integer `target` was bigger than `3`
  else

    ! get the length of the list of the prime numbers
    len_list = size(list, dim=1, kind=INT32)

    ! STEP.03
    ! judge whether the integer `target` is the multiple of `list(itr_elem)`
    do itr_elem = 1_INT32, len_list, 1_INT32

      if (IsMultiple(target=target, ref=list(itr_elem))) then
        ! STEP.BAD_END
        stat = .false.
        return
      end if

    end do

    ! STEP.TRUE_END
    stat = .true.
    return

  end if

end function IsPrime_list_INT32

pure function IsPrime_list_INT64 (target, list) result(stat)

  ! arguments for this <function>
  integer(kind=INT64), intent(in) :: target
  integer(kind=INT64), intent(in) :: list(:)

  ! return value of this <function>
  logical :: stat

  ! logical variables for this <function>
  integer(kind=INT64) :: itr_elem, len_list

  ! STEP.01
  ! if the integer `target` was less than `2`
  if (target .le. 1_INT64) then

    stat = .false.
    return

  ! STEP.02
  ! if the integer `target` was equal to `2` or `3`
  else if (target .le. 3_INT64) then

    stat = .true.
    return

  ! STEP.03
  ! if the integer `target` was bigger than `3`
  else

    ! get the length of the list of the prime numbers
    len_list = size(list, dim=1, kind=INT64)

    ! STEP.03
    ! judge whether the integer `target` is the multiple of `list(itr_elem)`
    do itr_elem = 1_INT64, len_list, 1_INT64

      if (IsMultiple(target=target, ref=list(itr_elem))) then
        ! STEP.BAD_END
        stat = .false.
        return
      end if

    end do

    ! STEP.TRUE_END
    stat = .true.
    return

  end if

end function IsPrime_list_INT64

! -------------------------------------------------------------------------------------------------------------------------------- !
! End of Source Code                                                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !
