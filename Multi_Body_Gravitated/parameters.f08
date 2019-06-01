module parameters_for_simulation

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! parameters for this simulation
  integer, parameter, public :: num_planets = 512

  real(REAL64), parameter, public :: val_mass_average    = 1.0e+000_REAL64
  real(REAL64), parameter, public :: val_mass_dispersion = 1.0e-001_REAL64

end module parameters_for_simulation