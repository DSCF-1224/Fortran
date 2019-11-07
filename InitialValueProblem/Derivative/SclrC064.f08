! repository   : Fortran / Initial Value Problem
! code created : 2018/09/15
! last updated : 2019/11/06

interface

  function fncOde ( data_sltn )

    ! <module>s to import
    use,     intrinsic :: iso_fortran_env
    use, non_intrinsic :: module_data_sltn

    ! require all variables to be explicitly declared
    implicit none

    ! arguments for this <function>
    type(type_sltn_SclrC064), intent(in) :: data_sltn

    ! return value of this <function>
    complex(REAL64) :: fncOde

  end function

end interface

! --- EOF --- !
