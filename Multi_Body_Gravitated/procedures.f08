module procedures_for_simulation

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: types_for_simulation
  use, non_intrinsic :: parameters_for_simulation
  use, non_intrinsic :: variables_for_simulation

  ! accessibility of the <subroutine>s and <function>s in this <module>
  public  :: main_procedures_for_simulation
  private :: initialize_data_planets_mass

  ! contained <subroutine>s and <function>s are below
  contains

  subroutine main_procedures_for_simulation
  end subroutine main_procedures_for_simulation



  subroutine initialize_data_planets_mass

    ! variables for this <subroutine>
    integer :: iter_planets


  end subroutine initialize_data_planets_mass

end module procedures_for_simulation



module procedures_random_numbers

  use, intrinsic :: iso_fortran_env

  ! accessibility of the <subroutine>s and <function>s in this <module>
  public  :: random_number_normal
  private :: 

  ! contained <subroutine>s and <function>s are below
  contains

  subroutine random_number_normal(retval)

    ! arguments for this <subroutine>
    real(REAL64), intent(inout) :: retval

    ! variables for this <subroutine>
    real(REAL64) :: randn1, randn2
    real(REAL64) :: rsq, fac

    rsq = huge(rsq)

    do while (rsq .ge. 1.0e+000_REAL64)
      call random_number(randn1)
      call random_number(randn2)
      randn1 = 2.0e+000_REAL64 * randn1 - 1.0e+000_REAL64
      randn2 = 2.0e+000_REAL64 * randn2 - 1.0e+000_REAL64
      rsq = randn1 * randn1 + randn2 * randn2
    end do

    fac    = sqrt(- 2.0e+000_REAL64 * log(rsq) / rsq)
    retval = randn2 * fac
    return

  end subroutine random_number_normal


end module procedures_random_numbers