module module_variables

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of contents in this <module>
  public  :: data_panel
  private :: data_panel_edge
  private :: data_panel_geometry

  ! <type>s for this <program>
  type data_panel_edge
    complex(REAL64), public :: left  ! left  edge of the panel
    complex(REAL64), public :: right ! right edge of the panel
  end type data_panel_edge

  type data_panel_vector
    complex(REAL64), public :: tangent
    complex(REAL64), public :: normal
  end type data_panel_vector
  
  type data_panel_geometry
    type   (data_panel_edge),   public :: edge
    complex(REAL64),            public :: midpoint ! middle point of the panel
    real   (REAL64),            public :: argument ! argument     of the panel
    real   (REAL64),            public :: length   ! length       of the panel
    type   (data_panel_vector), public :: vector
  end type data_panel_geometry

  type data_panel_strength
    real(REAL64), public :: source
    real(REAL64), public :: vortex
  end type data_panel_strength

  type data_panel
    type(data_panel_geometry), public :: geo
    type(data_panel_strength), public :: strength
  end type data_panel

  ! variables for this <module>
  type(data_panel), dimension(:), allocatable, public :: panels

end module module_variables