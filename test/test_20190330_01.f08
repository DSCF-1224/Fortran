!----------------------------------------------------------------------------------------------------------------------------------!
! [target]                                                                                                                         !
! Test miniparts\Test_Functions_for_Optimization.f08                                                                               !
!----------------------------------------------------------------------------------------------------------------------------------!
program main

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: mod_Test_Functions_for_Optimization

  ! require all variables to be explicitly declared
  implicit none

  ! constants for this <program>
  character(len=*, kind=1),  parameter :: path_folder_save =  "?"
  character(len=*, kind=1),  parameter :: name_file_save   =  "?"
  integer  (kind=INT32),     parameter :: num_samples_x    =  256_INT32
  integer  (kind=INT32),     parameter :: num_samples_y    =  num_samples_x
  integer  (kind=INT32),     parameter :: SAVE_UNIT        =  101
  real     (kind=REAL128),   parameter :: rcdx_min         = -5.0e+00_REAL128
  real     (kind=REAL128),   parameter :: rcdx_max         =  5.0e+00_REAL128
  real     (kind=REAL128),   parameter :: rcdy_min         =  rcdx_min
  real     (kind=REAL128),   parameter :: rcdy_max         =  rcdx_max
  real     (kind=REAL128),   parameter :: rcdx_diff        =  rcdx_max - rcdx_min
  real     (kind=REAL128),   parameter :: rcdy_diff        =  rcdy_max - rcdy_min

  ! variables for this <program>
  real(kind=REAL128)              :: rcdx_dist, rcdy_dist
  real(kind=REAL128), allocatable :: rcdx(:)
  real(kind=REAL128), allocatable :: rcdy(:)
  real(kind=REAL128), allocatable :: func(:,:)

  ! support variables for this <program>
  integer(kind=INT32) :: itr_x, itr_y, itr

  ! STEP.01
  ! allocate the arrays to store the values of test function
  allocate( rcdx(0:num_samples_x) )
  allocate( rcdy(0:num_samples_y) )
  allocate( func(0:num_samples_x, 0:num_samples_y) )

  ! STEP.02
  ! open the file to save the result
  open(unit=SAVE_UNIT, file=path_folder_save//name_file_save, action='write', status='replace')

  ! STEP.03
  ! calculate the coordinates of the sample points
  rcdx_dist = 1.0e+00_REAL128 / real(num_samples_x, kind=REAL128)
  rcdy_dist = 1.0e+00_REAL128 / real(num_samples_y, kind=REAL128)
  rcdx(:)   = (/ ( rcdx_min + rcdx_diff * real(itr, kind=REAL128) * rcdx_dist, itr = 0, num_samples_x, 1 ) /)
  rcdy(:)   = (/ ( rcdy_min + rcdy_diff * real(itr, kind=REAL128) * rcdy_dist, itr = 0, num_samples_y, 1 ) /)

  ! STEP.04
  ! calculate & save the test function
  do itr_x = 0, num_samples_x, 1
    do itr_y = 0, num_samples_y, 1

      ! func(itr_x,itr_y) = Bukin_Function_v6( rcdx(itr_x), rcdy(itr_y) )
      ! func(itr_x,itr_y) = Himmelblau_Function( rcdx(itr_x), rcdy(itr_y) )
      ! func(itr_x,itr_y) = Levi_Function_v13( rcdx(itr_x), rcdy(itr_y) )
      func(itr_x,itr_y) = ThreeHumpCamel_Function( rcdx(itr_x), rcdy(itr_y) )

      write(unit=SAVE_UNIT, fmt='(ES23.15e3,2(1X,ES23.15e3))', advance='yes') &!
        rcdx(itr_x), &!
        rcdy(itr_y), &!
        func(itr_x,itr_y)

    end do
    write(unit=SAVE_UNIT, fmt='(A)', advance='yes') ''
  end do

  ! STEP.05
  ! close the file to save the result
  close(unit=SAVE_UNIT, status='keep')

  print '(ES23.15e3)', minval( func(:,:) )
  print '(ES23.15e3)', maxval( func(:,:) )

  ! STEP.06
  ! deallocate the arrays to store the values of test function
  deallocate( rcdx )
  deallocate( rcdy )
  deallocate( func )
  
end program main

! -------------------------------------------------------------------------------------------------------------------------------- !
! End of Source Code                                                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !
