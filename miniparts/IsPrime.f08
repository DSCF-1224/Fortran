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
  public :: IsPrime_list

  private :: IsPrime_INT8,      IsPrime_INT16,      IsPrime_INT32,      IsPrime_INT64
  private :: IsPrime_list_INT8, IsPrime_list_INT16, IsPrime_list_INT32, IsPrime_list_INT64


  ! <interface>s for this <module>
  interface IsPrime
    module procedure IsPrime_INT8
    module procedure IsPrime_INT16
    module procedure IsPrime_INT32
    module procedure IsPrime_INT64
  end interface IsPrime

  interface IsPrime_list
    module procedure IsPrime_list_INT8
    module procedure IsPrime_list_INT16
    module procedure IsPrime_list_INT32
    module procedure IsPrime_list_INT64
  end interface IsPrime_list

  ! contained <subroutine>s and <function>s are below
  contains

  include "IsPrime\IsPrime.f08"
  include "IsPrime\IsPrime_list.f08"

end module mod_IsPrime

! -------------------------------------------------------------------------------------------------------------------------------- !
! End of Source Code                                                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !
