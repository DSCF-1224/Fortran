!----------------------------------------------------------------------------------------------------------------------------------!
! [target]                                                                                                                         !
! test of module <support_system_clock>                                                                                            !
!----------------------------------------------------------------------------------------------------------------------------------!
program test_20181220_02

  ! <module>s to iport
  use,     intrinsic :: iso_fortran_env       ! gfrotran 8.1.0
  use, non_intrinsic :: support_support       ! ver 20018.12.15
  use, non_intrinsic :: support_system_clock  ! ver 20018.12.20
  use, non_intrinsic :: support_date_and_time ! ver 20018.12.20

  ! constants for this <program>
  integer( kind=INT32 ), parameter :: num_elem  = 256_INT32
  integer( kind=INT32 ), parameter :: num_cycle =  10_INT32


  ! variables for this <program>
  real( kind=REAL64 ), allocatable :: val_REAL64(:,:,:)
  real( kind=REAL64 ), allocatable :: elapsed_time(:)
  real( kind=REAL64 )              :: elapsed_time_sum
  real( kind=REAL64 )              :: elapsed_time_avgf

  ! support variables for this <program>
  integer( kind=INT32 )    :: itrCycle, itrStage1, itrStage2, itrStage3
  type(Type_System_Clock)  :: process_start_SC, process_stop_SC
  type(Type_Date_And_Time) :: process_start_DT, process_stop_DT


  ! main process is below

  call Date_And_Time( process_start_DT )
  print '(A8,1X,A10,1X,A5,1X,I4,1X,I2,1X,I2,1X,I4,3(1X,I2),1X,I3)', process_start_DT

  allocate( val_REAL64( 1:num_elem, 1:num_elem, 1:num_elem ) )
  allocate( elapsed_time( 1:num_cycle ) )

  do itrCycle  = 1, num_cycle, 1

    val_REAL64(:,:,:) = 0.0e+000_REAL64

    call System_Clock( process_start_SC )

    do itrStage1 = 1, num_elem, 1
    do itrStage2 = 1, num_elem, 1
    do itrStage3 = 1, num_elem, 1
      val_REAL64( itrStage3, itrStage2, itrStage1 ) &!
        = val_REAL64( itrStage3, itrStage2, itrStage1 ) &!
        + real( itrStage1, kind= REAL64 ) &!
        + real( itrStage2, kind= REAL64 ) &!
        + real( itrStage3, kind= REAL64 )
    end do
    end do
    end do

    call System_Clock( process_stop_SC )

    elapsed_time( itrCycle ) = SYSTEM_CLOCK_ElapsedTime( start= process_start_SC, stop= process_stop_SC )

    write( unit= output_unit, fmt= '(I5,1X,ES25.17e3)', advance= 'yes' ) &!
      itrCycle, &!
      elapsed_time( itrCycle )

  end do

  elapsed_time_sum = sum( elapsed_time(:), dim= 1 )
  write( unit= output_unit, fmt= '(A,1X,ES25.17e3)', advance= 'yes' ) &!
    'Sum     :', elapsed_time_sum

  elapsed_time_avgf = elapsed_time_sum / real( num_cycle, kind= REAL64 )
  write( unit= output_unit, fmt= '(A,1X,ES25.17e3)', advance= 'yes' ) &!
    'Average :', elapsed_time_avgf

  elapsed_time(:)   = elapsed_time(:) - elapsed_time_avgf
  elapsed_time(:)   = elapsed_time(:) * elapsed_time(:)
  elapsed_time_sum  = sum( elapsed_time(:), dim= 1 )
  elapsed_time_avgf = sqrt( elapsed_time_sum / real( num_cycle, kind= REAL64 ) )

  elapsed_time_sum = sum( elapsed_time(:), dim= 1 )
  write( unit= output_unit, fmt= '(A,1X,ES25.17e3)', advance= 'yes' ) &!
    'RMS     :', elapsed_time_avgf

  deallocate( val_REAL64   )
  deallocate( elapsed_time )

  call Date_And_Time( process_stop_DT )
  print '(A8,1X,A10,1X,A5,1X,I4,1X,I2,1X,I2,1X,I4,3(1X,I2),1X,I3)', process_stop_DT
  call ReachedTheEnd

end program test_20181220_02