! repository   : Fortran / Initial Value Problem
! code created : 2019/11/06

include "DataType\module_data_sltn.f08"

module moduleIvpSolver

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: module_data_sltn

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of the <subroutine>s and <function>s in this <module>
  public  :: MthdEulerSclr     ! interface
  private :: MthdEulerSclrC032 ! function
  private :: MthdEulerSclrC064 ! function
  private :: MthdEulerSclrC128 ! function
  private :: MthdEulerSclrR032 ! function
  private :: MthdEulerSclrR064 ! function
  private :: MthdEulerSclrR128 ! function

  ! <interface>s for this <module>
  interface MthdEulerSclr
    module procedure :: MthdEulerSclrC032
    module procedure :: MthdEulerSclrC064
    module procedure :: MthdEulerSclrC128
    module procedure :: MthdEulerSclrR032
    module procedure :: MthdEulerSclrR064
    module procedure :: MthdEulerSclrR128
  end interface

  ! contained <subroutine>s and <function>s are below
  contains

  ! <interface>s for this <module>
  function MthdEulerSclrR032 ( func_eqn_diff, data_sltn_crnt, val_step_idvl ) result ( data_sltn_next )

    ! arguments for this <function>
    type(type_sltn_SclrR032), intent(in) :: data_sltn_crnt
    real(REAL32),             intent(in) :: val_step_idvl

    ! <function> as an argument of this <function>
    include "Derivative\SclrR032.f08"

    ! return value of this <function>
    type(type_sltn_SclrR032) :: data_sltn_next

    include "Method\Euler\Sclr.f08"

    return

  end function MthdEulerSclrR032

  function MthdEulerSclrR064 ( func_eqn_diff, data_sltn_crnt, val_step_idvl ) result ( data_sltn_next )

    ! arguments for this <function>
    type(type_sltn_SclrR064), intent(in) :: data_sltn_crnt
    real(REAL64),             intent(in) :: val_step_idvl

    ! <function> as an argument of this <function>
    include "Derivative\SclrR064.f08"

    ! return value of this <function>
    type(type_sltn_SclrR064) :: data_sltn_next

    include "Method\Euler\Sclr.f08"

    return

  end function MthdEulerSclrR064

  function MthdEulerSclrR128 ( func_eqn_diff, data_sltn_crnt, val_step_idvl ) result ( data_sltn_next )

    ! arguments for this <function>
    type(type_sltn_SclrR128), intent(in) :: data_sltn_crnt
    real(REAL128),            intent(in) :: val_step_idvl

    ! <function> as an argument of this <function>
    include "Derivative\SclrR128.f08"

    ! return value of this <function>
    type(type_sltn_SclrR128) :: data_sltn_next

    include "Method\Euler\Sclr.f08"

    return

  end function MthdEulerSclrR128

  function MthdEulerSclrC032 ( func_eqn_diff, data_sltn_crnt, val_step_idvl ) result ( data_sltn_next )

    ! arguments for this <function>
    type(type_sltn_SclrC032), intent(in) :: data_sltn_crnt
    real(REAL32),             intent(in) :: val_step_idvl

    ! <function> as an argument of this <function>
    include "Derivative\SclrC032.f08"

    ! return value of this <function>
    type(type_sltn_SclrC032) :: data_sltn_next

    include "Method\Euler\Sclr.f08"

    return

  end function MthdEulerSclrC032

  function MthdEulerSclrC064 ( func_eqn_diff, data_sltn_crnt, val_step_idvl ) result ( data_sltn_next )

    ! arguments for this <function>
    type(type_sltn_SclrC064), intent(in) :: data_sltn_crnt
    real(REAL64),             intent(in) :: val_step_idvl

    ! <function> as an argument of this <function>
    include "Derivative\SclrC064.f08"

    ! return value of this <function>
    type(type_sltn_SclrC064) :: data_sltn_next

    include "Method\Euler\Sclr.f08"

    return

  end function MthdEulerSclrC064

  function MthdEulerSclrC128 ( func_eqn_diff, data_sltn_crnt, val_step_idvl ) result ( data_sltn_next )

    ! arguments for this <function>
    type(type_sltn_SclrC128), intent(in) :: data_sltn_crnt
    real(REAL128),            intent(in) :: val_step_idvl

    ! <function> as an argument of this <function>
    include "Derivative\SclrC128.f08"

    ! return value of this <function>
    type(type_sltn_SclrC128) :: data_sltn_next

    include "Method\Euler\Sclr.f08"

    return

  end function MthdEulerSclrC128

end module moduleIvpSolver

! --- EOF --- !
