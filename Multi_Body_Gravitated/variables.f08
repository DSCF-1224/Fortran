module variables_for_simulation

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: types_for_simulation

  ! variables for simulation
  type(type_planet), allocatable, public :: data_planets(:)

  ! variables for error handing
  character(len=512, kind=1) :: buf_errmsg

end module variables_for_simulation