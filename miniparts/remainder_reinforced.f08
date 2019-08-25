! -------------------------------------------------------------------------------------------------------------------------------- !
! [target]                                                                                                                         !
! functions using intrinsic function `mod`                                                                                         !
! [test]                                                                                                                           !
! https://github.com/DSCF-1224/Fortran/blob/master/test/test_20190825_01.f08                                                       !
! -------------------------------------------------------------------------------------------------------------------------------- !

module remainder_reinforced

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of <subroutine>s and <function>s in this <module>
  public :: mod_sum
  public :: mod_mul

  private ::mod_sum_INT8,mod_sum_INT16,mod_sum_INT32,mod_sum_INT64
  private ::mod_mul_INT8,mod_mul_INT16,mod_mul_INT32,mod_mul_INT64

  ! <interface>s for this <module>
  interface mod_sum
    module procedure mod_sum_INT8
    module procedure mod_sum_INT16
    module procedure mod_sum_INT32
    module procedure mod_sum_INT64
  end interface mod_sum

  interface mod_mul
    module procedure mod_mul_INT8
    module procedure mod_mul_INT16
    module procedure mod_mul_INT32
    module procedure mod_mul_INT64
  end interface mod_mul

  ! contained <subroutine>s and <function>s are below
  contains

  ! Distributive

  pure function mod_sum_INT8 (a, b, p) result(mod_sum)

    ! arguments for this <function>
    integer(INT8), intent(in) :: a, b, p

    ! return value of this <function>
    integer(INT8) :: mod_sum

    mod_sum = mod(a = mod(a = a, p = p) + mod(a = b, p = p), p = p)
    return

  end function mod_sum_INT8

  pure function mod_sum_INT16 (a, b, p) result(mod_sum)

    ! arguments for this <function>
    integer(INT16), intent(in) :: a, b, p

    ! return value of this <function>
    integer(INT16) :: mod_sum

    mod_sum = mod(a = mod(a = a, p = p) + mod(a = b, p = p), p = p)
    return

  end function mod_sum_INT16

  pure function mod_sum_INT32 (a, b, p) result(mod_sum)

    ! arguments for this <function>
    integer(INT32), intent(in) :: a, b, p

    ! return value of this <function>
    integer(INT32) :: mod_sum

    mod_sum = mod(a = mod(a = a, p = p) + mod(a = b, p = p), p = p)
    return

  end function mod_sum_INT32

  pure function mod_sum_INT64 (a, b, p) result(mod_sum)

    ! arguments for this <function>
    integer(INT64), intent(in) :: a, b, p

    ! return value of this <function>
    integer(INT64) :: mod_sum

    mod_sum = mod(a = mod(a = a, p = p) + mod(a = b, p = p), p = p)
    return

  end function mod_sum_INT64

  pure function mod_mul_INT8 (a, b, p) result(mod_mul)

    ! arguments for this <function>
    integer(INT8), intent(in) :: a, b, p

    ! return value of this <function>
    integer(INT8) :: mod_mul

    mod_mul = mod(a = mod(a = a, p = p) * mod(a = b, p = p), p = p)
    return

  end function mod_mul_INT8

  pure function mod_mul_INT16 (a, b, p) result(mod_mul)

    ! arguments for this <function>
    integer(INT16), intent(in) :: a, b, p

    ! return value of this <function>
    integer(INT16) :: mod_mul

    mod_mul = mod(a = mod(a = a, p = p) * mod(a = b, p = p), p = p)
    return

  end function mod_mul_INT16

  pure function mod_mul_INT32 (a, b, p) result(mod_mul)

    ! arguments for this <function>
    integer(INT32), intent(in) :: a, b, p

    ! return value of this <function>
    integer(INT32) :: mod_mul

    mod_mul = mod(a = mod(a = a, p = p) * mod(a = b, p = p), p = p)
    return

  end function mod_mul_INT32

  pure function mod_mul_INT64 (a, b, p) result(mod_mul)

    ! arguments for this <function>
    integer(INT64), intent(in) :: a, b, p

    ! return value of this <function>
    integer(INT64) :: mod_mul

    mod_mul = mod(a = mod(a = a, p = p) * mod(a = b, p = p), p = p)
    return

  end function mod_mul_INT64

end module remainder_reinforced

! -------------------------------------------------------------------------------------------------------------------------------- !
! End of Source Code                                                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !
