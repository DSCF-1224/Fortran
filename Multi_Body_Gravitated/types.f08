module types_for_simulation

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! <type>s for this <module>
  type type_rect_cd
    real(REAL64), public :: x
    real(REAL64), public :: y
  end type type_rect_cd

  type type_planet
    real(REAL64),       public :: mass
    type(type_rect_cd), public :: acceleration
    type(type_rect_cd), public :: velocity
    type(type_rect_cd), public :: position
  end type type_planet

end module types_for_simulation