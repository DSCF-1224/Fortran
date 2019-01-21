!----------------------------------------------------------------------------------------------------------------------------------!
! [target]                                                                                                                         !
! test of module <Mod_Interpolation_1D>                                                                                            !
! test of module <Mod_Integration_1D>                                                                                              !
!----------------------------------------------------------------------------------------------------------------------------------!
program test_20190121_02

  ! <module>s to iport
  use,     intrinsic :: iso_fortran_env      ! gfrotran 8.1.0
  use, non_intrinsic :: support_support      !
  use, non_intrinsic :: support_io           !
  use, non_intrinsic :: Mod_Integration_1D   !
  use, non_intrinsic :: Mod_Interpolation_1D !

  ! constants for this <program>
  character(len=256, kind=1), parameter :: path_save     = 'GitHub\Fortran\test'
  character(len=256, kind=1), parameter :: filename_save = 'test_20190121_02.csv'

  integer( kind=INT32 ), parameter :: SAVE_UNIT  = 31_INT32
  integer( kind=INT32 ), parameter :: num_points = 512_INT32

  real( kind= REAL64), parameter :: CircCnst = acos(-1.0e+0_REAL64)


  ! variables for this <program>
  real( kind= REAL64 ), allocatable :: val_idvl(:)
  real( kind= REAL64 ), allocatable :: val_func(:)
  real( kind= REAL64 ), allocatable :: array_coef(:,:)


  ! support variables for this <program>
  integer( kind= INT32 )     :: itr
  integer( kind= INT32 )     :: buf_iostat
  real( kind= REAL64 )       :: buf_R064
  character(len=512, kind=1) :: buf_iomsg 


  ! STEP.01 !
  allocate( val_idvl(1:num_points) )
  allocate( val_func(1:num_points) )
  allocate( array_coef(1:3,1:num_points) )

  ! STEP.02.01 !
  val_idvl(:) = (/( real( itr-1_INT32 ), itr=1,num_points,1 )/)
  buf_R064    = 1.0e+0_REAL64/real( num_points-1_INT32, kind=REAL64 )
  val_idvl(:) = val_idvl(:) * buf_R064
  val_func(:) = (/( func_test( val_idvl(itr) ), itr=1,num_points,1 )/)

  ! STEP.02.02 ! adding noise
  do itr= 1,num_points,1
    call RANDOM_NUMBER( buf_R064 )
    val_func(itr) = val_func(itr) + 1.0e-4_REAL64 * ( buf_R064 - 5.0e-1_REAL64 )
  end do

  ! STEP.03 !
  call Calc_Coefficients_CubicSpline_FF(         &!
    num_points = num_points,                     &!
    array_idvl = val_idvl( 1:num_points ),       &!
    array_func = val_func( 1:num_points ),       &!
    array_coef = array_coef( 1:3, 1:num_points ) &!
  )!

  ! STEP.04 !
  print *, Calc_TrapezoidalIntegration_ByData( array_val=val_idvl(:), array_fnc=val_func(:) )
  print *, Calc_CubicSplineIntegration_ByData( array_val=val_idvl(:), array_fnc=val_func(:), array_coef=array_coef(:,:) )


  ! STEP.05 !
  open(                                                 &!
    unit   = SAVE_UNIT,                                 &!
    file   = trim(path_save)//'\'//trim(filename_save), &!
    iostat = buf_iostat,                                &!
    status = 'replace',                                 &!
    action = "write",                                   &!
    iomsg  = buf_iomsg                                  &!
  )
  call CheckIostatOpen( iostat= buf_iostat, silent= .true., iomsg= buf_iomsg )
  
  write( unit= SAVE_UNIT, fmt= '(A,2(",",A),3(",coef",I1))', advance= 'yes' ) &!
    'iterator', &!
    'IDVL',     &!
    'FUNC',     &!
    1,2,3

  do itr= 1, num_points, 1
    write( unit= SAVE_UNIT, fmt= '(I4,5(",",1X,ES23.15e3))', advance= 'yes' ) &!
      itr, val_idvl(itr), val_func(itr), array_coef(:,itr)
  end do

  close( unit=SAVE_UNIT, iostat= buf_iostat, iomsg= buf_iomsg )
  call CheckIostatClose( iostat= buf_iostat, iomsg= buf_iomsg, silent= .true. )

  ! STEP.06 !
  deallocate( val_idvl )
  deallocate( val_func )

  ! ! contained <subroutine>s and <function>s are below
  contains

  pure function func_test( idvl ) result( func )

    ! arguments for this <function>
    real( kind= REAL64 ), intent(in) :: idvl

    ! return value of this <function>
    real( kind= REAL64 ) :: func

    func = cos( idvl ); return

  end function func_test

end program test_20190121_02