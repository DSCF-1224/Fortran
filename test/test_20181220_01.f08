!----------------------------------------------------------------------------------------------------------------------------------!
! [target]                                                                                                                         !
! test of the binary I/O for gnuplot                                                                                               !
!----------------------------------------------------------------------------------------------------------------------------------!
program test_20181220_01

    ! <module>s to iport
    use,     intrinsic :: iso_fortran_env ! gfrotran 8.1.0
    use, non_intrinsic :: support_io

    ! constants for this <program>
    character(len=64, kind=1), parameter :: path_fldr_save = ! ???
    character(len=64, kind=1), parameter :: name_file_save = 'test.bin'

    integer( kind=INT32 ), parameter :: unit_save = 21
    integer( kind=INT32 ), parameter :: num_elem  = 1024


    ! variables for this <program>
    real( kind=REAL64 ) :: rcdx

    ! support variables for this <program>
    character( len=128, kind=1 ) :: buf_iomsg
    integer( kind=INT32 )        :: itr
    integer( kind=INT32 )        :: buf_iostat


    open( &!
        unit   = unit_save, &!
        file   = trim(path_fldr_save)//'/'//trim(name_file_save), &!
        form   = 'unformatted', &!
        status = 'replace', &!
        action = 'write', &!
        access = 'stream', &!
        iostat = buf_iostat, &!
        iomsg  = buf_iomsg &!
    )
    call CheckIostatOpen( iostat= buf_iostat, iomsg= buf_iomsg, silent= .false. )

    do itr = 0, num_elem, 1
        rcdx = real( 10*itr, kind= REAL64 ) / real( num_elem, kind= REAL64 ) 
        write( unit= unit_save, iostat= buf_iostat ) rcdx, sin( acos( -1.0e+00_REAL64 ) * rcdx * rcdx )
        call CheckIostatWrite( iostat= buf_iostat, silent= .true. )
    end do

    close( unit= unit_save, status= 'keep' )
    call CheckIostatClose( iostat= buf_iostat, iomsg= buf_iomsg, silent= .false. )

    call ReachedTheEnd

    ! [GNUPLOT]
    ! plot 'test.bin' binary format="%float64%float64" with lines, sin(pi*x*x)

end program test_20181220_01