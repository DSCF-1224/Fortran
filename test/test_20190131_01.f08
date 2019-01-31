!----------------------------------------------------------------------------------------------------------------------------------!
! [target]                                                                                                                         !
! How to use the OpenMP                                                                                                            !
! [reference]                                                                                                                      !
! http://www.nag-j.co.jp/openMP/openMPHelloOpenMP.html#HelloOpenMP                                                                 !
!----------------------------------------------------------------------------------------------------------------------------------!
program test_20190131_01

  ! <module>s to iport
  use, intrinsic :: iso_fortran_env ! gfrotran 8.1.0
  !$ use omp_lib                    ! use OpenMP

  ! require all variables to be explicitly declared
  implicit none

  ! STEP.01
  print '(A)', '[Start of Process]'

  ! STEP.02
  ! parallel process
  !$omp parallel
  print '(A,I2,1X,A,I2)', "Total:", omp_get_num_threads(), "ID:", omp_get_thread_num()

  !$omp end parallel
  def

  ! STEP.03
  print '(A)', '[End of Process]'

end program test_20190131_01