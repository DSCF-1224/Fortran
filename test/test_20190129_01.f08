!----------------------------------------------------------------------------------------------------------------------------------!
! [target]                                                                                                                         !
! test of the binary I/O for gnuplot                                                                                               !
!----------------------------------------------------------------------------------------------------------------------------------!
module test_20190129_01

  ! <module>s to iport
  use,     intrinsic :: iso_fortran_env ! gfrotran 8.1.0
  use, non_intrinsic :: support_io

  ! require all variables to be explicitly declared
  implicit none

  public :: test_01
  public :: test_02

  ! constants for this <module>
  character(len=64, kind=1), parameter, private :: path_fldr_save = 'D:\gfortran'
  character(len=64, kind=1), parameter, private :: name_file_save = 'test.bin'

  integer( kind=INT32 ), parameter, private :: unit_save = 21
  integer( kind=INT32 ), parameter, private :: num_elem  = 64

  real( kind=REAL64 ), parameter, private :: CircCnst = acos(-1.0e+0_REAL64)


  ! contained <subroutine>s and <function>s are below
  contains

  ! test of `binary format="%2double"` 
  subroutine test_01

    ! variables for this <module>
    real( kind=REAL64 ) :: rcdx

    ! support variables for this <module>
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
      write( unit= unit_save, iostat= buf_iostat ) rcdx, sin( CircCnst * rcdx )
      call CheckIostatWrite( iostat= buf_iostat, silent= .true. )
    end do

    close( unit= unit_save, status= 'keep' )
    call CheckIostatClose( iostat= buf_iostat, iomsg= buf_iomsg, silent= .false. )

    ! [GNUPLOT]
    ! plot "test.bin" binary format="%2double"

  end subroutine test_01

  ! test of `binary format="%3double"` 
  subroutine test_02

    ! variables for this <module>
    real( kind=REAL64 ) :: rcdx

    ! support variables for this <module>
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
      write( unit= unit_save, iostat= buf_iostat ) rcdx
      write( unit= unit_save, iostat= buf_iostat ) cos( CircCnst * rcdx )
      write( unit= unit_save, iostat= buf_iostat ) sin( -rcdx )
      call CheckIostatWrite( iostat= buf_iostat, silent= .true. )
    end do

    close( unit= unit_save, status= 'keep' )
    call CheckIostatClose( iostat= buf_iostat, iomsg= buf_iomsg, silent= .false. )

    ! [GNUPLOT]
    ! plot "test.bin" binary format="%2double"

  end subroutine test_02

  ! test of `binary format="%3double"` 
  subroutine test_03

    ! variables for this <module>
    real( kind=REAL32 ), allocatable :: rcdx(:), rcdy(:), func(:,:)

    ! support variables for this <module>
    character( len=128, kind=1 ) :: buf_iomsg
    integer( kind=INT32 )        :: itr_x, itr_y
    integer( kind=INT32 )        :: buf_iostat
    real( kind=REAL32 )          :: buf_R32


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
    
    allocate( rcdx(0:num_elem) )
    allocate( rcdy(0:num_elem) )
    allocate( func(0:num_elem,0:num_elem) )

    rcdx(:) = (/ ( real( itr_x, kind= REAL32 ), itr_x= 0, num_elem, 1 ) /)
    buf_R32 = 1.0e+0_REAL32 / real( num_elem, kind= REAL32 )
    rcdx(:) = rcdx(:) * buf_R32
    rcdy(:) = rcdx(:)

    do itr_y = 0, num_elem, 1
    do itr_x = 0, num_elem, 1
      func(itr_x,itr_y) = rcdx(itr_x)
    end do
    end do

    do itr_y = 0, num_elem, 1
    do itr_x = 0, num_elem, 1
      write( unit= unit_save, iostat= buf_iostat ) func(itr_x,itr_y)
    end do
    end do

    deallocate( rcdx )
    deallocate( rcdy )
    deallocate( func )

    close( unit= unit_save, status= 'keep' )
    call CheckIostatClose( iostat= buf_iostat, iomsg= buf_iomsg, silent= .false. )

    ! [GNUPLOT]
    ! plot "test.bin" binary format="%3double"

  end subroutine test_03

end module test_20190129_01



program main

    ! <module>s to import
    use, non_intrinsic :: support_support
    use, non_intrinsic :: test_20190129_01

    ! require all variables to be explicitly declared
    implicit none

    ! STEP.01/01 !
    call test_03
    call ReachedTheEnd

end program main
