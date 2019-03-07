! -------------------------------------------------------------------------------------------------------------------------------- !
! [target]                                                                                                                         !
! judge whether the loaded integer is a prime number                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !

module mod_IsPrime

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: mod_IsMultiple

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of <subroutine>s and <function>s in this <module>
  public :: IsPrime

  private :: IsPrime_INT8, IsPrime_INT16, IsPrime_INT32, IsPrime_INT64


  ! <interface>s for this <module>
  interface IsPrime
    module procedure IsPrime_INT8
    module procedure IsPrime_INT16
    module procedure IsPrime_INT32
    module procedure IsPrime_INT64
  end interface IsPrime

  ! contained <subroutine>s and <function>s are below
  contains

  include "IsPrime\IsPrime.f08"

end module mod_IsPrime

! -------------------------------------------------------------------------------------------------------------------------------- !
! End of Source Code                                                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !
