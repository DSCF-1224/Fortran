! repository   : Fortran / Initial Value Problem
! code created : 2019/11/06

module module_data_sltn

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of the <subroutine>s and <function>s in this <module>
  public :: type_sltn_SclrC032
  public :: type_sltn_SclrC064
  public :: type_sltn_SclrC128
  public :: type_sltn_SclrR032
  public :: type_sltn_SclrR064
  public :: type_sltn_SclrR128

  type type_sltn_SclrC032
    integer         :: step = 0
    real   (REAL32) :: idvl = 0.0_REAL32
    complex(REAL32) :: sltn = cmplx( x = 0.0_REAL32, y = 0.0_REAL32, kind = REAL32 )
  end type

  type type_sltn_SclrC064
    integer         :: step = 0
    real   (REAL64) :: idvl = 0.0_REAL64
    complex(REAL64) :: sltn = cmplx( x = 0.0_REAL64, y = 0.0_REAL64, kind = REAL64 )
  end type

  type type_sltn_SclrC128
    integer          :: step = 0
    real   (REAL128) :: idvl = 0.0_REAL128
    complex(REAL128) :: sltn = cmplx( x = 0.0_REAL128, y = 0.0_REAL128, kind = REAL128 )
  end type

  type type_sltn_SclrR032
    integer         :: step = 0
    real   (REAL32) :: idvl = 0.0_REAL32
    real   (REAL32) :: sltn = 0.0_REAL32
  end type

  type type_sltn_SclrR064
    integer         :: step = 0
    real   (REAL64) :: idvl = 0.0_REAL64
    real   (REAL64) :: sltn = 0.0_REAL64
  end type

  type type_sltn_SclrR128
    integer          :: step = 0
    real   (REAL128) :: idvl = 0.0_REAL128
    real   (REAL128) :: sltn = 0.0_REAL128
  end type

end module module_data_sltn

! --- EOF --- !
