! code created : 2018/09/15
! last updated : 2019/11/06

interface

  pure function func_eqn_diff ( dmsn, idvl, sltn )

    ! <module>s to import
    use, intrinsic :: iso_fortran_env

    ! require all variables to be explicitly declared
    implicit none

    ! arguments for this <function>
    integer,         intent(in), optional :: dmsn
    real   (REAL32), intent(in), optional :: idvl
    real   (REAL32), intent(in), optional :: sltn(1:dmsn)

    ! return value of this <function>
    real(REAL32) :: func_eqn_diff

  end function

end interface

! --- EOF --- !
