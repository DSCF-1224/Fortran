! -------------------------------------------------------------------------------------------------------------------------------- !
! [target]                                                                                                                         !
! judge whether the loaded integer is a multiple of the another loaded integer                                                     !
! -------------------------------------------------------------------------------------------------------------------------------- !

module mod_IsMultiple

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of <subroutine>s and <function>s in this <module>
  public :: IsMultiple
  public :: IsEven
  public :: IsOdd

  private :: IsMultiple_INT8, IsMultiple_INT16, IsMultiple_INT32, IsMultiple_INT64
  private :: IsEven_INT8,     IsEven_INT16,     IsEven_INT32,     IsEven_INT64


  ! <interface>s for this <module>
  interface IsMultiple
    module procedure IsMultiple_INT8
    module procedure IsMultiple_INT16
    module procedure IsMultiple_INT32
    module procedure IsMultiple_INT64
  end interface IsMultiple

  interface IsEven
    module procedure IsEven_INT8
    module procedure IsEven_INT16
    module procedure IsEven_INT32
    module procedure IsEven_INT64
  end interface IsEven

  interface IsOdd
    module procedure IsOdd_INT8
    module procedure IsOdd_INT16
    module procedure IsOdd_INT32
    module procedure IsOdd_INT64
  end interface IsOdd

  ! contained <subroutine>s and <function>s are below
  contains

  include "IsMultiple\IsMultiple.f08"
  include "IsMultiple\IsEven.f08"
  include "IsMultiple\IsOdd.f08"

end module mod_IsMultiple

! -------------------------------------------------------------------------------------------------------------------------------- !
! End of Source Code                                                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !
