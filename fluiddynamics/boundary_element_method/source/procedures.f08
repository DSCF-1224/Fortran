module module_procedures

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: module_parameters
  use, non_intrinsic :: module_variables

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of contents in this <module>
  private :: allocation_data_panel    ! subroutine
  private :: deallocation_data_panel  ! subroutine
  private :: calc_data_panel_points   ! subroutine
  private :: calc_data_panel_geometry ! subroutine

  ! local variables for this <module>
  complex(REAL64), dimension(:), allocatable, private :: edge_point

  ! contained <subroutine>s and <function>s are below
  contains

  subroutine allocation_data_panel

    allocate( panels    (1_INT32:num_panel        ) )
    allocate( edge_point(1_INT32:num_panel+1_INT32) )
    return
  
  end subroutine allocation_data_panel


  subroutine deallocation_data_panel

    deallocate(panels    )
    deallocate(edge_point)
    return
  
  end subroutine deallocation_data_panel


  subroutine calc_data_panel_points

    ! variables for this <subroutine>
    real(REAL64) :: step_argument
    real(REAL64) :: argument

    ! support variables for this <subroutine>
    integer(INT32) :: itr

    ! STEP.01
    ! calculate the step of the argument of the edge points
    step_argument = CircCnst2 / real(num_panel, kind=REAL64)

    ! STEP.02
    ! calculate the coordinates of the edge points before Joukowski transformed
    do itr = 1_INT32, num_panel, 1_INT32
      argument        = step_argument * (itr - 1_INT32)
      argument        = step_argument * 5.0e-001_REAL64 + argument
      edge_point(itr) = cmplx( cos(argument), sin(argument), kind=REAL64 )
    end do

    edge_point(num_panel + 1_INT32) = edge_point(1_INT32)

    ! STEP.03
    ! save the coordinates of the edge points after Joukowski transform
    do itr = 1_INT32, num_panel + 1_INT32, 1_INT32
      panels(itr)%geo%edge%left  = edge_point(itr          )
      panels(itr)%geo%edge%right = edge_point(itr + 1_INT32)
    end do

    ! STEP.END
    return

  end subroutine calc_data_panel_points


  subroutine calc_data_panel_geometry

    ! STEP.01
    ! calculate the midpoint of the each panel
    panels(:)%geo%midpoint = 5.0e-001_REAL64 * ( panels(:)%geo%edge%left + panels(:)%geo%edge%right )

    ! STEP.02
    ! calculate the tangent vector of each panel
    ! (which are not normalized)
    panels(:)%geo%vector%tangent = panels(:)%geo%edge%right - panels(:)%geo%edge%left

    ! STEP.03
    ! calculate the length of the each panel
    panels(:)%geo%length = abs( panels(:)%geo%vector%tangent )

    ! STEP.04
    ! normalize the tangent vector of each panel
    panels(:)%geo%vector%tangent = panels(:)%geo%vector%tangent / panels(:)%geo%length

    ! STEP.05
    ! calculate the tangent vector of each panel
    ! (which are normalized)
    panels(:)%geo%vector%normal = ImagUnit * panels(:)%geo%vector%tangent

    ! STEP.06
    ! calculate the argument of each panel
    panels(:)%geo%argument

    ! STEP.END
    return

  end subroutine calc_data_panel_geometry

end module module_procedures