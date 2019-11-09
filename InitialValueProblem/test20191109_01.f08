include "module_SolverIvp.f08"

program test20191109_01

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: moduleIvpSolver

  ! require all variables to be explicitly declared
  implicit none

  ! constants for this <program>
  character(len=*, kind=1), parameter :: path_folder_save = 'E:\Arc-2\test20191109_01'
  integer,                  parameter :: unit_save        = 21
  
  ! <type>s for this <program>
  type type_parameters
    character(len=128, kind=1) :: name_file_save
    integer                    :: num_step_end
    real(REAL64)               :: val_step_idvl
  end type

  ! variables for this <program>
  type(type_parameters) :: dataParameter

  ! STEP.01
  ! 20 steps
  dataParameter%name_file_save = 'Euler01'
  dataParameter%num_step_end   = 20
  dataParameter%val_step_idvl  = 5.00e-001_REAL64
  call test_Method ( dataParameter )

  ! STEP.02
  ! 40 steps
  dataParameter%name_file_save = 'Euler02'
  dataParameter%num_step_end   = 40
  dataParameter%val_step_idvl  = 2.50e-001_REAL64
  call test_Method ( dataParameter )

  ! STEP.03
  ! 80 steps
  dataParameter%name_file_save = 'Euler03'
  dataParameter%num_step_end   = 80
  dataParameter%val_step_idvl  = 1.250e-001_REAL64
  call test_Method ( dataParameter )

  ! STEP.03
  ! 160 steps
  dataParameter%name_file_save = 'Euler04'
  dataParameter%num_step_end   = 160
  dataParameter%val_step_idvl  = 6.125e-002_REAL64
  call test_Method ( dataParameter )

  ! contained <subroutine>s and <function>s are below
  contains

  subroutine test_Method ( dataParameter )

    ! arguments for this <subroutine>
    type(type_parameters), intent(in) :: dataParameter

    ! variables for this <subroutine>
    type(type_sltn_SclrR064) :: sltnCrnt
    type(type_sltn_SclrR064) :: sltnPre

    ! STEP.01
    ! open the file to save the numerical solution
    open(&!
      unit   = unit_save,                                &!
      access = 'stream',                                 &!
      action = 'write',                                  &!
      file   = generate_path_file_save( dataParameter ), &!
      form   = 'unformatted',                            &!
      status = 'replace'                                 &!
    )

    ! STEP.02
    ! set the initial condition
    sltnCrnt%step = 0
    sltnCrnt%idvl = 0.0e+000_REAL64
    sltnCrnt%sltn = 1.0e+000_REAL64

    ! STEP.03
    ! compute & save the numerical solution
    do

      call write_solution( sltnCrnt )

      if ( sltnCrnt%step .ge. dataParameter%num_step_end ) then
        exit
      end if

      sltnPre = sltnCrnt

      call MthdEulerSclr( eqn, sltnPre, dataParameter%val_step_idvl, sltnCrnt )

    end do

    ! STEP.04
    ! close the file saved the numerical solution
    close(unit=unit_save, status='keep')

    ! STEP.END
    return

  end subroutine

  pure function generate_path_file_save ( dataParameter ) result ( path_file_save )

    ! arguments for this <subroutine>
    type(type_parameters), intent(in) :: dataParameter

    ! return value of this <function>
    character(len=256, kind=1) :: path_file_save

    path_file_save = path_folder_save // '\' // trim(dataParameter%name_file_save) // '.bin'

    return

  end function generate_path_file_save

  subroutine write_solution ( dataSltn )

    ! arguments for this <subroutine>
    type(type_sltn_SclrR064), intent(in) :: dataSltn

    write(unit=unit_save) dataSltn%idvl, dataSltn%sltn, relative_error_sltn( dataSltn )

    return

  end subroutine write_solution

  pure function eqn ( dataSltn ) result ( val_sltn )

    ! arguments for this <function>
    type(type_sltn_SclrR064), intent(in) :: dataSltn

    ! return value of this <function>
    real(REAL64) :: val_sltn

    val_sltn = dataSltn%sltn * cos( dataSltn%idvl )

    return

  end function eqn

  pure function sltn_exact ( dataSltn ) result ( val_sltn_exact )

    ! arguments for this <function>
    type(type_sltn_SclrR064), intent(in) :: dataSltn

    ! return value of this <function>
    real(REAL64) :: val_sltn_exact

    val_sltn_exact = exp( sin( dataSltn%idvl ) )

    return

  end function sltn_exact

  pure function relative_error_sltn ( dataSltn ) result ( val_error )

    ! arguments for this <function>
    type(type_sltn_SclrR064), intent(in) :: dataSltn

    ! return value of this <function>
    real(REAL64) :: val_error

    ! variables for this <function>
    real(REAL64) :: val_sltn_exact

    val_sltn_exact = sltn_exact ( dataSltn ) 
    val_error      = abs( dataSltn%sltn - val_sltn_exact ) / val_sltn_exact

    return

  end function relative_error_sltn

end program test20191109_01