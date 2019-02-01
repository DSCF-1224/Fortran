!----------------------------------------------------------------------------------------------------------------------------------!
! [target]                                                                                                                         !
! How to use the OpenMP                                                                                                            !
! How to use GNUPLOT                                                                                                             !
! [reference]                                                                                                                      !
! http://www.nag-j.co.jp/openMP/openMPHelloOpenMP.html#HelloOpenMP                                                                 !
!----------------------------------------------------------------------------------------------------------------------------------!
module test_20190201_01

  ! <module>s to iport
  use, intrinsic :: iso_fortran_env ! gfrotran 8.1.0
  !$ use omp_lib                    ! use OpenMP

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of <subroutine>s and <function>s in this <module>
  public  :: MainProcess ! subroutine
  private :: EachProcess ! subroutine

  ! constants for this <program>
  character(len= 35, kind= 1), parameter :: path_fldr_gnuplot = "C:\Program Files (x86)\gnuplot\bin\"


  ! contained <subroutine>s and <function>s are below
  contains


  subroutine MainProcess

    ! STEP.01
    ! output the status of the process
    print '(A)', '[Start of Process]'

    ! STEP.02
    ! parallel process for GNUPLOT
    !$omp parallel num_threads(2) default( private )
    !$omp sections
      
      !$omp section ! thread 01/04
        print '(A,I2,1X,A,I2)', "Total:", omp_get_num_threads(), "ID:", omp_get_thread_num()
        call EachProcess( "test_20190201_01_01.plt" )
      
      !$omp section ! thread 02/04
        print '(A,I2,1X,A,I2)', "Total:", omp_get_num_threads(), "ID:", omp_get_thread_num()
        call EachProcess( "test_20190201_01_02.plt" )
    
    !$omp end sections
    !$omp end parallel

    ! STEP.03
    ! output the status of the process
    print '(A)', '[End of Process]'


  end subroutine MainProcess


  subroutine EachProcess( path_file_plt )

    ! arguments for this <subroutine>
    character( len=*, kind=1 ), intent(in) :: path_file_plt

    ! STEP.01/01
      call execute_command_line(&!
        'CD /D "'//path_fldr_gnuplot//'" & gnuplot '//trim(path_file_plt)&!
      )

    ! STEP.TRUE_END
    return

  end subroutine EachProcess


end module test_20190201_01



program main

  ! <module>s to iport
  use,     intrinsic :: iso_fortran_env  ! gfrotran 8.1.0
  !$ use omp_lib                         ! use OpenMP
  use, non_intrinsic :: test_20190201_01

  ! require all variables to be explicitly declared
  implicit none

  call MainProcess

end program main