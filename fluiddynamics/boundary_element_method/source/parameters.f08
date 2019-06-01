module module_parameters

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! require all variables to be explicitly declared
  implicit none

  integer(INT32 ), parameter, public :: num_panel         = 32_INT32        ! number    of the panels
  real   (REAL64), parameter, public :: velocity_uniform  = 1.0e+000_REAL64 ! velocity  of the uniform flow
  real   (REAL64), parameter, public :: elevation_uniform = 0.0e+000_REAL64 ! elevation of the uniform flow

  complex(REAL64), parameter, public :: ImagUnit  = cmplx(0.0e+000_REAL64, 1.0e+000_REAL64, kind=REAL64)
  real   (REAL64), parameter, public :: CircCnst  = acos(-1.0e+000_REAL64)
  real   (REAL64), parameter, public :: CircCnst2 = 2.0e+000_REAL64 * CircCnst

end module module_parameters