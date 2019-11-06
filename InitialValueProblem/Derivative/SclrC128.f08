! repository   : Fortran / Initial Value Problem
! code created : 2018/09/15
! last updated : 2019/11/06

interface

  pure function func_eqn_diff ( data_sltn )

    ! <module>s to import
    use,     intrinsic :: iso_fortran_env
    use, non_intrinsic :: module_data_sltn

    ! require all variables to be explicitly declared
    implicit none

    ! arguments for this <function>
    type(type_sltn_SclrC128), intent(in) :: data_sltn

    ! return value of this <function>
    complex(REAL128) :: func_eqn_diff

  end function

end interface

! --- EOF --- !
