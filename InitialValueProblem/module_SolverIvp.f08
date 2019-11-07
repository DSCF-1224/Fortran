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

  public  :: MthdEulerModSclr     ! interface
  private :: MthdEulerModSclrC032 ! function
  private :: MthdEulerModSclrC064 ! function
  private :: MthdEulerModSclrC128 ! function
  private :: MthdEulerModSclrR032 ! function
  private :: MthdEulerModSclrR064 ! function
  private :: MthdEulerModSclrR128 ! function

  ! <interface>s for this <module>
  interface MthdEulerSclr
    module procedure :: MthdEulerSclrC032
    module procedure :: MthdEulerSclrC064
    module procedure :: MthdEulerSclrC128
    module procedure :: MthdEulerSclrR032
    module procedure :: MthdEulerSclrR064
    module procedure :: MthdEulerSclrR128
  end interface

  interface MthdEulerModSclr
    module procedure :: MthdEulerModSclrC032
    module procedure :: MthdEulerModSclrC064
    module procedure :: MthdEulerModSclrC128
    module procedure :: MthdEulerModSclrR032
    module procedure :: MthdEulerModSclrR064
    module procedure :: MthdEulerModSclrR128
  end interface

  ! contained <subroutine>s and <function>s are below
  contains

  ! --- Euler Method --- !

  function MthdEulerSclrR032 ( fncOde, dataCrnt, val_step_idvl ) result ( dataNext )

    ! arguments for this <function>
    type(type_sltn_SclrR032), intent(in) :: dataCrnt
    real(REAL32),             intent(in) :: val_step_idvl

    ! <function> as an argument of this <function>
    include "Derivative\SclrR032.f08"

    ! return value of this <function>
    type(type_sltn_SclrR032) :: dataNext

    include "Method\Euler\Sclr.f08"

    return

  end function MthdEulerSclrR032

  function MthdEulerSclrR064 ( fncOde, dataCrnt, val_step_idvl ) result ( dataNext )

    ! arguments for this <function>
    type(type_sltn_SclrR064), intent(in) :: dataCrnt
    real(REAL64),             intent(in) :: val_step_idvl

    ! <function> as an argument of this <function>
    include "Derivative\SclrR064.f08"

    ! return value of this <function>
    type(type_sltn_SclrR064) :: dataNext

    include "Method\Euler\Sclr.f08"

    return

  end function MthdEulerSclrR064

  function MthdEulerSclrR128 ( fncOde, dataCrnt, val_step_idvl ) result ( dataNext )

    ! arguments for this <function>
    type(type_sltn_SclrR128), intent(in) :: dataCrnt
    real(REAL128),            intent(in) :: val_step_idvl

    ! <function> as an argument of this <function>
    include "Derivative\SclrR128.f08"

    ! return value of this <function>
    type(type_sltn_SclrR128) :: dataNext

    include "Method\Euler\Sclr.f08"

    return

  end function MthdEulerSclrR128

  function MthdEulerSclrC032 ( fncOde, dataCrnt, val_step_idvl ) result ( dataNext )

    ! arguments for this <function>
    type(type_sltn_SclrC032), intent(in) :: dataCrnt
    real(REAL32),             intent(in) :: val_step_idvl

    ! <function> as an argument of this <function>
    include "Derivative\SclrC032.f08"

    ! return value of this <function>
    type(type_sltn_SclrC032) :: dataNext

    include "Method\Euler\Sclr.f08"

    return

  end function MthdEulerSclrC032

  function MthdEulerSclrC064 ( fncOde, dataCrnt, val_step_idvl ) result ( dataNext )

    ! arguments for this <function>
    type(type_sltn_SclrC064), intent(in) :: dataCrnt
    real(REAL64),             intent(in) :: val_step_idvl

    ! <function> as an argument of this <function>
    include "Derivative\SclrC064.f08"

    ! return value of this <function>
    type(type_sltn_SclrC064) :: dataNext

    include "Method\Euler\Sclr.f08"

    return

  end function MthdEulerSclrC064

  function MthdEulerSclrC128 ( fncOde, dataCrnt, val_step_idvl ) result ( dataNext )

    ! arguments for this <function>
    type(type_sltn_SclrC128), intent(in) :: dataCrnt
    real(REAL128),            intent(in) :: val_step_idvl

    ! <function> as an argument of this <function>
    include "Derivative\SclrC128.f08"

    ! return value of this <function>
    type(type_sltn_SclrC128) :: dataNext

    include "Method\Euler\Sclr.f08"

    return

  end function MthdEulerSclrC128

  ! --- Modified Euler Method --- !

  function MthdEulerModSclrR032 ( fncOde, dataCrnt, val_step_idvl ) result ( dataNext )

    ! arguments for this <function>
    type(type_sltn_SclrR032), intent(in) :: dataCrnt
    real(REAL32),             intent(in) :: val_step_idvl

    ! <function> as an argument of this <function>
    include "Derivative\SclrR032.f08"

    ! variables for this <function>
    type(type_sltn_SclrR032) :: dataBffr
    real(REAL32)             :: val_step_idvl_half

    ! return value of this <function>
    type(type_sltn_SclrR032) :: dataNext

    val_step_idvl_half = 5.0e-1_REAL32 * val_step_idvl

    include "Method\EulerMod\Sclr.f08"

    return

  end function MthdEulerModSclrR032

  function MthdEulerModSclrR064 ( fncOde, dataCrnt, val_step_idvl ) result ( dataNext )

    ! arguments for this <function>
    type(type_sltn_SclrR064), intent(in) :: dataCrnt
    real(REAL64),             intent(in) :: val_step_idvl

    ! <function> as an argument of this <function>
    include "Derivative\SclrR064.f08"

    ! variables for this <function>
    type(type_sltn_SclrR064) :: dataBffr
    real(REAL64)             :: val_step_idvl_half

    ! return value of this <function>
    type(type_sltn_SclrR064) :: dataNext

    val_step_idvl_half = 5.0e-1_REAL64 * val_step_idvl

    include "Method\EulerMod\Sclr.f08"

    return

  end function MthdEulerModSclrR064

  function MthdEulerModSclrR128 ( fncOde, dataCrnt, val_step_idvl ) result ( dataNext )

    ! arguments for this <function>
    type(type_sltn_SclrR128), intent(in) :: dataCrnt
    real(REAL128),            intent(in) :: val_step_idvl

    ! <function> as an argument of this <function>
    include "Derivative\SclrR128.f08"

    ! variables for this <function>
    type(type_sltn_SclrR128) :: dataBffr
    real(REAL128)            :: val_step_idvl_half

    ! return value of this <function>
    type(type_sltn_SclrR128) :: dataNext

    val_step_idvl_half = 5.0e-1_REAL128 * val_step_idvl

    include "Method\EulerMod\Sclr.f08"

    return

  end function MthdEulerModSclrR128

  function MthdEulerModSclrC032 ( fncOde, dataCrnt, val_step_idvl ) result ( dataNext )

    ! arguments for this <function>
    type(type_sltn_SclrC032), intent(in) :: dataCrnt
    real(REAL32),             intent(in) :: val_step_idvl

    ! <function> as an argument of this <function>
    include "Derivative\SclrC032.f08"

    ! variables for this <function>
    type(type_sltn_SclrC032) :: dataBffr
    real(REAL32)             :: val_step_idvl_half

    ! return value of this <function>
    type(type_sltn_SclrC032) :: dataNext

    val_step_idvl_half = 5.0e-1_REAL32 * val_step_idvl

    include "Method\EulerMod\Sclr.f08"

    return

  end function MthdEulerModSclrC032

  function MthdEulerModSclrC064 ( fncOde, dataCrnt, val_step_idvl ) result ( dataNext )

    ! arguments for this <function>
    type(type_sltn_SclrC064), intent(in) :: dataCrnt
    real(REAL64),             intent(in) :: val_step_idvl

    ! <function> as an argument of this <function>
    include "Derivative\SclrC064.f08"

    ! variables for this <function>
    type(type_sltn_SclrC064) :: dataBffr
    real(REAL64)             :: val_step_idvl_half

    ! return value of this <function>
    type(type_sltn_SclrC064) :: dataNext

    val_step_idvl_half = 5.0e-1_REAL64 * val_step_idvl

    include "Method\EulerMod\Sclr.f08"

    return

  end function MthdEulerModSclrC064

  function MthdEulerModSclrC128 ( fncOde, dataCrnt, val_step_idvl ) result ( dataNext )

    ! arguments for this <function>
    type(type_sltn_SclrC128), intent(in) :: dataCrnt
    real(REAL128),            intent(in) :: val_step_idvl

    ! <function> as an argument of this <function>
    include "Derivative\SclrC128.f08"

    ! variables for this <function>
    type(type_sltn_SclrC128) :: dataBffr
    real(REAL128)            :: val_step_idvl_half

    ! return value of this <function>
    type(type_sltn_SclrC128) :: dataNext

    val_step_idvl_half = 5.0e-1_REAL128 * val_step_idvl

    include "Method\EulerMod\Sclr.f08"

    return

  end function MthdEulerModSclrC128

end module moduleIvpSolver

! --- EOF --- !
