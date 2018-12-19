program test_20181219_01

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env ! gfortran 8.1.0
  use, non_intrinsic :: support_support ! version 2018.12.15
  use, non_intrinsic :: support_io      ! version 2018.12.19

  ! constants for this <program>
  integer(kind=int32) :: unit_inpt = 21

  ! variables for this <program>

    character(len=256, kind=1) :: path_target =
    character(len=256, kind=1) :: buf_iomsg

    integer(kind=int32) :: buf_iostat

    real(kind=real64) :: buf_real64


  open(&!
    unit   = unit_inpt, &!
    file   = , &!
    iostat = buf_iostat, &!
    iomsg  = buf_iomsg, &!
    action = 'read', &!
    status = 'old' &!
  )

  call CheckIostatOpen( iostat= buf_iostat, iomsg= buf_iomsg, silent= .false. )

  do
    read( unit= unit_inpt, fmt= *, iostat= buf_iostat, iomsg= buf_iomsg ) buf_real64

    if( IS_IOSTAT_END( buf_iostat ) ) then
      exit
    else

      call CheckIostatRead( iostat= buf_iostat, iomsg= buf_iomsg, silent= .true. )

      write( unit= output_unit, fmt= '(ES23.15e3)', advance= 'yes', iostat= buf_iostat, iomsg= buf_iomsg ) buf_real64
      call CheckIostatWrite( iostat= buf_iostat, iomsg= buf_iomsg, silent= .true. )

    end if

  end do

  close( unit= unit_inpt, status='keep', iostat= buf_iostat, iomsg= buf_iomsg  )
  call CheckIostatClose( iostat= buf_iostat, iomsg= buf_iomsg, silent= .false. )

  call ReachedTheEnd

end program test_20181219_01