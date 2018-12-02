!-------------------------------------------------------------------------------------------------!
! [COMPILER]                                                                                      !
!   gforrtran 8.1.0                                                                               !
! [PURPOSE]                                                                                       !
!   [1/2]                                                                                         !
!   confirm how to use an allocatable array                                                       !
!   [2/2]                                                                                         !
!   how to use <MT95> module                                                                      !
!-------------------------------------------------------------------------------------------------!
program main

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: mt95
  use, non_intrinsic :: support_io
  use, non_intrinsic :: support_support

  ! require all variables to be explicitly declared
  implicit none

  ! constants for this <program>
  integer, parameter :: num_points_1 = 1000
  integer, parameter :: num_points_2 = num_points_1

  ! variables for this <program>
  real(real64), allocatable :: rand(:,:)

  ! support variables for this <program>
  integer :: itr2
  integer :: statval

  allocate( &!
    rand( 1:num_points_2, 1:num_points_1  ), &!
    stat = statval, errmsg = buf_ErrMsg_io  &!
  )
  call CheckStatAllocate( stat= statval, errmsg= buf_ErrMsg_io )

  call genrand_init( put= 1 )
  call genrand_real3( rand )

  do itr2 = 1, min(num_points_2,10), 1
    write( unit= output_unit, fmt= '(1024(es13.5e3,1X))', advance= 'yes' ) &!
      rand( itr2, 1:min(num_points_1,5) )
  end do

  deallocate( rand, stat = statval, errmsg = buf_ErrMsg_io )
  call CheckStatDeallocate( stat= statval, errmsg= buf_ErrMsg_io )

  call ReachedTheEnd

end program main