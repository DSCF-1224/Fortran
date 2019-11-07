include "module_SolverIvp.f08"

program test201911_01

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: moduleIvpSolver

  ! require all variables to be explicitly declared
  implicit none

  ! constants for this <program>
  integer, parameter :: unit_save = 21
  
  ! <type>s for this <program>
  type type_parameters
    character(len=128, kind=1) :: path_file_save ! 'E:\Arc-2\test20191107_01\Euler.bin'
    integer                    :: num_step_end
    real(REAL64)               :: val_step_idvl
  end type

  ! variables for this <program>
  type(type_parameters) :: dataParameter

  ! STEP.01
  ! 10 steps
  dataParameter%path_file_save = 'E:\Arc-2\test20191107_01\Euler01.bin'
  dataParameter%num_step_end   = 10 ** 1
  dataParameter%val_step_idvl  = 1.0e-001_REAL64
  call test_MthdEulerSclr ( dataParameter )

  ! STEP.02
  ! 100 steps
  dataParameter%path_file_save = 'E:\Arc-2\test20191107_01\Euler02.bin'
  dataParameter%num_step_end   = 10 ** 2
  dataParameter%val_step_idvl  = 1.0e-002_REAL64
  call test_MthdEulerSclr ( dataParameter )

  ! STEP.03
  ! 1000 steps
  dataParameter%path_file_save = 'E:\Arc-2\test20191107_01\Euler03.bin'
  dataParameter%num_step_end   = 10 ** 3
  dataParameter%val_step_idvl  = 1.0e-003_REAL64
  call test_MthdEulerSclr ( dataParameter )

  ! contained <subroutine>s and <function>s are below
  contains

  subroutine test_MthdEulerSclr ( dataParameter )

    ! arguments for this <subroutine>
    type(type_parameters), intent(in) :: dataParameter

    ! variables for this <subroutine>
    type(type_sltn_SclrR064) :: sltnCrnt
    type(type_sltn_SclrR064) :: sltnPre

    ! STEP.01
    ! open the file to save the numerical solution
    open(&!
      unit   = unit_save,                            &!
      access = 'stream',                             &!
      action = 'write',                              &!
      file   = trim( dataParameter%path_file_save ), &!
      form   = 'unformatted',                        &!
      status = 'replace'                             &!
    )

    ! STEP.02
    ! set the initial condition
    sltnCrnt%step = 0
    sltnCrnt%idvl = 0.0e+000_REAL64
    sltnCrnt%sltn = 0.0e+000_REAL64

    ! STEP.03
    ! compute & save the numerical solution
    do

      call write_solution( sltnCrnt )

      if ( sltnCrnt%step .ge. dataParameter%num_step_end ) then
        exit
      end if

      sltnPre  = sltnCrnt
      sltnCrnt = MthdEulerSclr( eqn, sltnPre, dataParameter%val_step_idvl )

    end do

    ! STEP.04
    ! close the file saved the numerical solution
    close(unit=unit_save, status='keep')

    ! STEP.END
    return

  end subroutine

  subroutine write_solution ( dataSltn )

    ! arguments for this <subroutine>
    type(type_sltn_SclrR064), intent(in) :: dataSltn

    write(unit=unit_save) dataSltn%idvl, dataSltn%sltn

    return

  end subroutine write_solution

  pure function eqn ( dataSltn ) result ( val_sltn )

    ! arguments for this <function>
    type(type_sltn_SclrR064), intent(in) :: dataSltn

    ! return value of this <function>
    real(REAL64) :: val_sltn

    val_sltn = 1.0e+000_REAL64 - dataSltn%sltn * dataSltn%sltn

    return

  end function eqn

end program test201911_01