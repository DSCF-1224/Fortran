! code created : 2019.01.20
! code updated : 2019.03.05
!
! compiler for test : GNU Fortran 8.1.0
!
! [reference]
! https://gihyo.jp/book/2018/978-4-7741-9690-9
! pp.82-83

module GCD_LCM

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! require all variables to be explicitly declared
  implicit none


  ! accessibility of <subroutine>s and <function>s in this <module>
  public  :: GCD_RCR
  public  :: LCM_RCR
  private :: GCD_RCR_INT08, GCD_RCR_INT16, GCD_RCR_INT32, GCD_RCR_INT64
  private :: LCM_RCR_INT08, LCM_RCR_INT16, LCM_RCR_INT32, LCM_RCR_INT64

  ! <interface>s for this <module>
  interface GCD_RCR
    module procedure GCD_RCR_INT08
    module procedure GCD_RCR_INT16
    module procedure GCD_RCR_INT32
    module procedure GCD_RCR_INT64
  end interface GCD_RCR

  interface GCD_IND
    module procedure GCD_IND_INT08
    module procedure GCD_IND_INT16
    module procedure GCD_IND_INT32
    module procedure GCD_IND_INT64
  end interface GCD_IND

  interface LCM_RCR
    module procedure LCM_RCR_INT08
    module procedure LCM_RCR_INT16
    module procedure LCM_RCR_INT32
    module procedure LCM_RCR_INT64
  end interface LCM_RCR

  interface LCM_IND
    module procedure LCM_IND_INT08
    module procedure LCM_IND_INT16
    module procedure LCM_IND_INT32
    module procedure LCM_IND_INT64
  end interface LCM_IND

  ! <subroutine>s and <function>s in this <module> is below
  contains

  ! greatest common divsor (recursion) !
  include "GreatestCommonDivisor\recursive.f08"

  ! greatest common divsor (non-recursion) !
  include "GreatestCommonDivisor\non-recursive.f08"

  ! least common multiple (recursive) !
  include "LeastCommonMultiple\recursive.f08"

  ! least common multiple (non-recursive) !
  include "LeastCommonMultiple\non-recursive.f08"

end module