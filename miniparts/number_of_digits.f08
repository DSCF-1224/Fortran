! -------------------------------------------------------------------------------------------------------------------------------- !
! [target]                                                                                                                         !
! It is to compute the number of digits in integer `target` in base 10
! -------------------------------------------------------------------------------------------------------------------------------- !
module number_of_digits

  ! ⟨module⟩s to import
  use, intrinsic :: iso_fortran_env

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of <subroutine>s and <function>s in this <module>
  public  :: number_of_digits_INT8
  public  :: number_of_digits_INT16
  public  :: number_of_digits_INT32
  public  :: number_of_digits_INT64
  private :: number_of_digits_INT8_to_INT8
  private :: number_of_digits_INT16_to_INT8
  private :: number_of_digits_INT32_to_INT8
  private :: number_of_digits_INT64_to_INT8
  private :: number_of_digits_INT8_to_INT16
  private :: number_of_digits_INT16_to_INT16
  private :: number_of_digits_INT32_to_INT16
  private :: number_of_digits_INT64_to_INT16
  private :: number_of_digits_INT8_to_INT32
  private :: number_of_digits_INT16_to_INT32
  private :: number_of_digits_INT32_to_INT32
  private :: number_of_digits_INT64_to_INT32
  private :: number_of_digits_INT8_to_INT64
  private :: number_of_digits_INT16_to_INT64
  private :: number_of_digits_INT32_to_INT64
  private :: number_of_digits_INT64_to_INT64

  ! <interface>s in this <module>
  interface  number_of_digits_INT8
    module procedure  number_of_digits_INT8_to_INT8
    module procedure  number_of_digits_INT16_to_INT8
    module procedure  number_of_digits_INT32_to_INT8
    module procedure  number_of_digits_INT64_to_INT8
  end interface

  interface  number_of_digits_INT16
    module procedure  number_of_digits_INT8_to_INT16
    module procedure  number_of_digits_INT16_to_INT16
    module procedure  number_of_digits_INT32_to_INT16
    module procedure  number_of_digits_INT64_to_INT16
  end interface

  interface  number_of_digits_INT32
    module procedure  number_of_digits_INT8_to_INT32
    module procedure  number_of_digits_INT16_to_INT32
    module procedure  number_of_digits_INT32_to_INT32
    module procedure  number_of_digits_INT64_to_INT32
  end interface

  interface  number_of_digits_INT64
    module procedure  number_of_digits_INT8_to_INT64
    module procedure  number_of_digits_INT16_to_INT64
    module procedure  number_of_digits_INT32_to_INT64
    module procedure  number_of_digits_INT64_to_INT64
  end interface

  ! contained ⟨subroutine⟩s and ⟨function⟩s are below
  contains

  !  input : INT8
  ! output : INT8
  pure function number_of_digits_INT8_to_INT8 (target) result (ndigits)

    ! arguments for this ⟨function⟩
    integer (kind=INT8), intent(in) :: target

    ! return value of this <function>
    integer (kind=INT8) :: ndigits

    ndigits = int (log10 (real(target, kind=REAL32)), kind=INT8) + 1_INT8
    return

  end function number_of_digits_INT8_to_INT8


  !  input : INT16
  ! output : INT8
  pure function number_of_digits_INT16_to_INT8 (target) result (ndigits)

    ! arguments for this ⟨function⟩
    integer (kind=INT16), intent(in) :: target

    ! return value of this <function>
    integer (kind=INT8) :: ndigits

    ndigits = int (log10 (real(target, kind=REAL32)), kind=INT8) + 1_INT8
    return

  end function number_of_digits_INT16_to_INT8


  !  input : INT32
  ! output : INT8
  pure function number_of_digits_INT32_to_INT8 (target) result (ndigits)

    ! arguments for this ⟨function⟩
    integer (kind=INT32), intent(in) :: target

    ! return value of this <function>
    integer (kind=INT8) :: ndigits

    ndigits = int (log10 (real(target, kind=REAL32)), kind=INT8) + 1_INT8
    return

  end function number_of_digits_INT32_to_INT8


  !  input : INT64
  ! output : INT8
  pure function number_of_digits_INT64_to_INT8 (target) result (ndigits)

    ! arguments for this ⟨function⟩
    integer (kind=INT64), intent(in) :: target

    ! return value of this <function>
    integer (kind=INT8) :: ndigits

    ndigits = int (log10 (real(target, kind=REAL32)), kind=INT8) + 1_INT8
    return

  end function number_of_digits_INT64_to_INT8

  !  input : INT8
  ! output : INT16
  pure function number_of_digits_INT8_to_INT16 (target) result (ndigits)

    ! arguments for this ⟨function⟩
    integer (kind=INT8), intent(in) :: target

    ! return value of this <function>
    integer (kind=INT16) :: ndigits

    ndigits = int (log10 (real(target, kind=REAL32)), kind=INT16) + 1_INT16
    return

  end function number_of_digits_INT8_to_INT16


  !  input : INT16
  ! output : INT16
  pure function number_of_digits_INT16_to_INT16 (target) result (ndigits)

    ! arguments for this ⟨function⟩
    integer (kind=INT16), intent(in) :: target

    ! return value of this <function>
    integer (kind=INT16) :: ndigits

    ndigits = int (log10 (real(target, kind=REAL32)), kind=INT16) + 1_INT16
    return

  end function number_of_digits_INT16_to_INT16


  !  input : INT32
  ! output : INT16
  pure function number_of_digits_INT32_to_INT16 (target) result (ndigits)

    ! arguments for this ⟨function⟩
    integer (kind=INT32), intent(in) :: target

    ! return value of this <function>
    integer (kind=INT16) :: ndigits

    ndigits = int (log10 (real(target, kind=REAL32)), kind=INT16) + 1_INT16
    return

  end function number_of_digits_INT32_to_INT16


  !  input : INT64
  ! output : INT16
  pure function number_of_digits_INT64_to_INT16 (target) result (ndigits)

    ! arguments for this ⟨function⟩
    integer (kind=INT64), intent(in) :: target

    ! return value of this <function>
    integer (kind=INT16) :: ndigits

    ndigits = int (log10 (real(target, kind=REAL32)), kind=INT16) + 1_INT16
    return

  end function number_of_digits_INT64_to_INT16


  !  input : INT8
  ! output : INT32
  pure function number_of_digits_INT8_to_INT32 (target) result (ndigits)

    ! arguments for this ⟨function⟩
    integer (kind=INT8), intent(in) :: target

    ! return value of this <function>
    integer (kind=INT32) :: ndigits

    ndigits = int (log10 (real(target, kind=REAL32)), kind=INT32) + 1_INT32
    return

  end function number_of_digits_INT8_to_INT32


  !  input : INT16
  ! output : INT32
  pure function number_of_digits_INT16_to_INT32 (target) result (ndigits)

    ! arguments for this ⟨function⟩
    integer (kind=INT16), intent(in) :: target

    ! return value of this <function>
    integer (kind=INT32) :: ndigits

    ndigits = int (log10 (real(target, kind=REAL32)), kind=INT32) + 1_INT32
    return

  end function number_of_digits_INT16_to_INT32


  !  input : INT32
  ! output : INT32
  pure function number_of_digits_INT32_to_INT32 (target) result (ndigits)

    ! arguments for this ⟨function⟩
    integer (kind=INT32), intent(in) :: target

    ! return value of this <function>
    integer (kind=INT32) :: ndigits

    ndigits = int (log10 (real(target, kind=REAL32)), kind=INT32) + 1_INT32
    return

  end function number_of_digits_INT32_to_INT32


  !  input : INT64
  ! output : INT32
  pure function number_of_digits_INT64_to_INT32 (target) result (ndigits)

    ! arguments for this ⟨function⟩
    integer (kind=INT64), intent(in) :: target

    ! return value of this <function>
    integer (kind=INT32) :: ndigits

    ndigits = int (log10 (real(target, kind=REAL32)), kind=INT32) + 1_INT32
    return

  end function number_of_digits_INT64_to_INT32


  !  input : INT8
  ! output : INT64
  pure function number_of_digits_INT8_to_INT64 (target) result (ndigits)

    ! arguments for this ⟨function⟩
    integer (kind=INT8), intent(in) :: target

    ! return value of this <function>
    integer (kind=INT64) :: ndigits

    ndigits = int (log10 (real(target, kind=REAL32)), kind=INT64) + 1_INT64
    return

  end function number_of_digits_INT8_to_INT64


  !  input : INT16
  ! output : INT64
  pure function number_of_digits_INT16_to_INT64 (target) result (ndigits)

    ! arguments for this ⟨function⟩
    integer (kind=INT16), intent(in) :: target

    ! return value of this <function>
    integer (kind=INT64) :: ndigits

    ndigits = int (log10 (real(target, kind=REAL32)), kind=INT64) + 1_INT64
    return

  end function number_of_digits_INT16_to_INT64


  !  input : INT32
  ! output : INT64
  pure function number_of_digits_INT32_to_INT64 (target) result (ndigits)

    ! arguments for this ⟨function⟩
    integer (kind=INT32), intent(in) :: target

    ! return value of this <function>
    integer (kind=INT64) :: ndigits

    ndigits = int (log10 (real(target, kind=REAL32)), kind=INT64) + 1_INT64
    return

  end function number_of_digits_INT32_to_INT64


  !  input : INT64
  ! output : INT64
  pure function number_of_digits_INT64_to_INT64 (target) result (ndigits)

    ! arguments for this ⟨function⟩
    integer (kind=INT64), intent(in) :: target

    ! return value of this <function>
    integer (kind=INT64) :: ndigits

    ndigits = int (log10 (real(target, kind=REAL32)), kind=INT64) + 1_INT64
    return

  end function number_of_digits_INT64_to_INT64

end module number_of_digits
