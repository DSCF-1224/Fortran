! [purpose of this program]
! test `miniparts\Test_Functions_for_Optimization.f08`

program test_20190526_01

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: mod_Test_Functions_for_Optimization

  ! require all variables to be explicitly declared
  implicit none

  ! <type>s for this <program>
  type data_range
    real(REAL64) :: min
    real(REAL64) :: max
  end type data_range

  type data_domain
    type(data_range) :: cdx
    type(data_range) :: cdy
  end type data_domain

  type data_coordinate
    real(REAL64) :: cdx
    real(REAL64) :: cdy
  end type data_coordinate

  type data_num_interval
    integer(INT32) :: cdx
    integer(INT32) :: cdy
  end type data_num_interval

  ! constants for this <program>
  integer(INT32), parameter :: UNIT_SAVE = 100_INT32

  type(data_num_interval), parameter :: num_interval &!
    = data_num_interval(1024_INT32, 1024_INT32)

  type(data_domain), parameter :: domain_test &!
    = data_domain(&!
      data_range(-5.00e+00_REAL64, +5.00e+00_REAL64), &!
      data_range(-5.00e+00_REAL64, +5.00e+00_REAL64)  &!
    )

  ! variables for this <program>
  type(data_coordinate) :: step
  
  ! arrays for this <program>
  real(REAL64), dimension(:), allocatable :: cdx
  real(REAL64), dimension(:), allocatable :: cdy

  ! support variables for this <program>
  integer(INT32) :: itr
  integer(INT32) :: itr_cdx
  integer(INT32) :: itr_cdy

  ! STEP.01
  ! open the file to save the result
  open(&!
    unit   = UNIT_SAVE,                    &!
    file   = '???\test_20190526_01.gpbin', &!
    form   = 'unformatted',                &!
    access = 'stream',                     &!
    action = 'write',                      &!
    status = 'replace'                     &!
  )

  ! STEP.02
  ! allocate the array to store the data of test function
  allocate( cdx(0_INT32:num_interval%cdx) )
  allocate( cdy(0_INT32:num_interval%cdy) )

  ! STEP.03
  ! calculate the coordinate of discrete points for test function
  step%cdx = (domain_test%cdx%max - domain_test%cdx%min) / num_interval%cdx
  step%cdy = (domain_test%cdy%max - domain_test%cdy%min) / num_interval%cdy

  do itr = 0_INT32, num_interval%cdx, 1_INT32
    cdx(itr) = domain_test%cdx%min + itr * step%cdx
  end do

  do itr = 0_INT32, num_interval%cdy, 1_INT32
    cdy(itr) = domain_test%cdy%min + itr * step%cdy
  end do

  ! STEP.04
  ! calculate & save the return value of test function
  do itr_cdy = 0_INT32, num_interval%cdy, 1_INT32
  do itr_cdx = 0_INT32, num_interval%cdx, 1_INT32
    write(unit=UNIT_SAVE) &!
      cdx(itr_cdx), &!
      cdy(itr_cdy), &!
      ThreeHumpCamel_Func ( cdx(itr_cdx), cdy(itr_cdy) )
  end do
  end do

  ! STEP.05
  ! allocate the array to store the data of test function
  deallocate( cdx )
  deallocate( cdy )

  ! STEP.06
  ! close the file to save the return value of the test function
  close(unit=UNIT_SAVE, status='keep')

end program test_20190526_01

! End of File !
