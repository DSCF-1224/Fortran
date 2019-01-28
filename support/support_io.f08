!----------------------------------------------------------------------------------------------------------------------------------!
! [target]                                                                                                                         !
! check return value of <STAT> or <STATVAL>                                                                                        !
!----------------------------------------------------------------------------------------------------------------------------------!
module support_io

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env ! gfortran 8.1.0
  use, non_intrinsic :: support_support ! version 2018.12.15

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of <subroutine>s and <function>s in this <module>
  public :: CheckStatAllocate
  public :: CheckStatDeallocate
  public :: CheckIostatClose
  public :: CheckIostatOpen
  public :: CheckIostatRead
  public :: CheckIostatWrite

  private :: CheckStatAllocateDeallocate
  private :: CheckIostatOpenClose
  private :: CheckIostatReadWrite


  ! constants for this <module>
  character( len=  8, kind= 1 ), parameter, private :: name_statement_ALLOCATE   = 'ALLOCATE'
  character( len= 10, kind= 1 ), parameter, private :: name_statement_DEALLOCATE = 'DE'//name_statement_ALLOCATE
  character( len=  5, kind= 1 ), parameter, private :: name_statement_CLOSE      = 'CLOSE'
  character( len=  4, kind= 1 ), parameter, private :: name_statement_OPEN       = 'OPEN'
  character( len=  4, kind= 1 ), parameter, private :: name_statement_READ       = 'READ'
  character( len=  5, kind= 1 ), parameter, private :: name_statement_WRITE      = 'WRITE'
  character( len= 19, kind= 1 ), parameter, private :: str_ItHasSucceededTo      = 'It has succeeded to'
  character( len= 24, kind= 1 ), parameter, private :: str_TheValueOfIostatIs    = 'The value of <IOSTAT> is'

  integer( kind= int32 ), parameter, private :: mode_CheckIostatAllocateDeallocate_ALLOCATE   = -1_int32
  integer( kind= int32 ), parameter, private :: mode_CheckIostatAllocateDeallocate_DEALLOCATE =  1_int32

  integer( kind= int32 ), parameter, private :: mode_CheckIostatReadWrite_READ  = -1_int32
  integer( kind= int32 ), parameter, private :: mode_CheckIostatReadWrite_WRITE =  1_int32

  integer( kind= int32 ), parameter, private :: mode_CheckIostatOpenClose_CLOSE = -1_int32
  integer( kind= int32 ), parameter, private :: mode_CheckIostatOpenClose_OPEN  =  1_int32

  ! variables for this <module>
  character( len=len_ErrMsg ), public :: buf_ErrMsg_io


  ! <subroutine>s and <function>s for this <module>
  contains


  ! check <stat> of <allocate> and <deallocate> statement
  subroutine CheckStatAllocateDeallocate( stat, errmsg, silent, mode )

    ! arguments for this <subroutine>
    integer( kind=int32 ),      intent(in)           :: stat
    character( len=*, kind=1 ), intent(in), optional :: errmsg
    logical,                    intent(in)           :: silent
    integer( kind=int32 ),      intent(in)           :: mode

    ! local variables for this <subroutine>
    character( len=10, kind=1 ) :: name_statement_buf

    select case( mode )
      case( mode_CheckIostatAllocateDeallocate_ALLOCATE   ); name_statement_buf = name_statement_ALLOCATE
      case( mode_CheckIostatAllocateDeallocate_DEALLOCATE ); name_statement_buf = name_statement_DEALLOCATE
    end select

    select case( stat )

      case( 0_int32 )

        if( .not. silent ) then
          call PrintOnConsoleStatement( name_statement_ALLOCATE )
          call PrintOnConsoleStatus
          write( unit=output_unit, fmt='(A,1X,A,1X,A)', advance='yes' ) str_ItHasSucceededTo, name_statement_buf, 'the array.'
        end if

        return  ! TRUE_END

      case default

        call PrintOnConsoleStatement( name_statement_ALLOCATE )
        call PrintOnConsoleStatus

        write( unit=output_unit, fmt='(A,1X)', advance='no' ) 'An'

        select case( stat )
          case(1_int32:)
            write( unit=output_unit, fmt='(A,1X)', advance='no' ) 'unrecoverable'
          case(:-1_int32)
            write( unit=output_unit, fmt='(A,1X)', advance='no' ) 'undefined'
        end select

        write( unit=output_unit, fmt='(A)',       advance='yes' ) 'error was detected !'
        write( unit=output_unit, fmt='(A,1X,I8)', advance='yes' ) 'The value of <STAT> is', stat
        
        if( present(errmsg) ) then
          call PrintOnConsoleErrMsg
          write( unit=output_unit, fmt='(A)', advance='yes' ) trim(errmsg)
        end if
        
        call StopWithMessage  ! BAD_END

    end select

  end subroutine CheckStatAllocateDeallocate


  ! check <stat> of <allocate> statement
  subroutine CheckStatAllocate( stat, errmsg, silent )

    ! arguments for this <subroutine>
    integer(kind=int32),                 intent(in)           :: stat
    character( len=len_ErrMsg, kind=1 ), intent(in), optional :: errmsg
    logical,                             intent(in), optional :: silent

    ! local variable for this <subroutine>
    logical :: buf_silent

    if( .not. present( silent ) ) then
      buf_silent = .false.
    else
      buf_silent = silent
    end if

    if( present( errmsg ) ) then

      call CheckStatAllocateDeallocate( &!
        stat   = stat,                                       &!
        errmsg = errmsg,                                     &!
        silent = buf_silent,                                 &!
        mode   = mode_CheckIostatAllocateDeallocate_ALLOCATE &!
      )

    else

      call CheckStatAllocateDeallocate( &!
        stat   = stat,                                       &!
        silent = buf_silent,                                 &!
        mode   = mode_CheckIostatAllocateDeallocate_ALLOCATE &!
      )

    end if

  end subroutine


  ! check <stat> of <allocate> statement
  subroutine CheckStatDeallocate( stat, errmsg, silent )

    ! arguments for this <subroutine>
    integer(kind=int32),                 intent(in)           :: stat
    character( len=len_ErrMsg, kind=1 ), intent(in), optional :: errmsg
    logical,                             intent(in), optional :: silent

    ! local variable for this <subroutine>
    logical :: buf_silent

    if( .not. present( silent ) ) then
      buf_silent = .false.
    else
      buf_silent = silent
    end if

    if( present( errmsg ) ) then

      call CheckStatAllocateDeallocate( &!
        stat   = stat,                                         &!
        errmsg = errmsg,                                       &!
        silent = buf_silent,                                   &!
        mode   = mode_CheckIostatAllocateDeallocate_DEALLOCATE &!
      )

    else

      call CheckStatAllocateDeallocate( &!
        stat   = stat,                                         &!
        silent = buf_silent,                                   &!
        mode   = mode_CheckIostatAllocateDeallocate_DEALLOCATE &!
      )

    end if

  end subroutine


  ! check <iostat> of <close> and <open> statement
  subroutine CheckIostatOpenClose( iostat, iomsg, silent, mode )

    ! arguments for this <subroutine>
    integer( kind=int32 ),      intent(in)           :: iostat
    character( len=*, kind=1 ), intent(in), optional :: iomsg
    logical,                    intent(in)           :: silent
    integer( kind=int32 ),      intent(in)           :: mode

    ! local variables for this <subroutine>
    character( len=5, kind=1 ) :: name_statement_buf

    select case( mode )
      case( mode_CheckIostatOpenClose_CLOSE ); name_statement_buf = name_statement_CLOSE
      case( mode_CheckIostatOpenClose_OPEN  ); name_statement_buf = name_statement_OPEN
    end select

    select case ( iostat )

      case ( 0_int32 )

        if( .not. silent ) then

          call PrintOnConsoleStatement( name_statement_buf )
          call PrintOnConsoleStatus
          write( unit= output_unit, fmt= '(A,1X,A,1X,A,/)', advance= 'yes' ) &!
            str_ItHasSucceededTo, trim( name_statement_buf ), 'the target file.'

        end if

      case default

        call PrintOnConsoleStatement( trim( name_statement_buf ) )
        call PrintOnConsoleStatus

        write( unit=output_unit, fmt='(A,/,A,1X,I0)', advance='yes' ) &!
          'An error was detected.', &!
          str_TheValueOfIostatIs,      &!
          iostat

        if( present( iomsg ) ) then
          call PrintOnConsoleErrMsg
          write( unit= output_unit, fmt='(A,/)', advance= 'yes' ) trim( iomsg )
        end if

        call StopWithMessage

    end select

  end subroutine


  ! check <iostat> of <CLOSE> statement
  subroutine CheckIostatClose( iostat, iomsg, silent )

    ! arguments for this <subroutine>
    integer( kind=int32 ),      intent(in)           :: iostat
    character( len=*, kind=1 ), intent(in), optional :: iomsg
    logical,                    intent(in), optional :: silent

    ! local variable for this <subroutine>
    logical :: buf_silent

    if( .not. present( silent ) ) then
      buf_silent = .false.
    else
      buf_silent = silent
    end if

    if( present( iomsg ) ) then

      call CheckIostatOpenClose( &!
        iostat = iostat,                         &!
        iomsg  = iomsg,                          &!
        silent = buf_silent,                     &!
        mode   = mode_CheckIostatOpenClose_CLOSE &!
      )

    else

      call CheckIostatOpenClose( &!
        iostat = iostat,                         &!
        silent = buf_silent,                     &!
        mode   = mode_CheckIostatOpenClose_CLOSE &!
      )

    end if

  end subroutine


  ! check <iostat> of <OPEN> statement
  subroutine CheckIostatOpen( iostat, iomsg, silent )

    ! arguments for this <subroutine>
    integer( kind=int32 ),      intent(in)           :: iostat
    character( len=*, kind=1 ), intent(in), optional :: iomsg
    logical,                    intent(in), optional :: silent

    ! local variable for this <subroutine>
    logical :: buf_silent

    if( .not. present( silent ) ) then
      buf_silent = .false.
    else
      buf_silent = silent
    end if

    if( present( iomsg ) ) then

      call CheckIostatOpenClose( &!
        iostat = iostat,                        &!
        iomsg  = iomsg,                         &!
        silent = buf_silent,                    &!
        mode   = mode_CheckIostatOpenClose_OPEN &!
      )

    else

      call CheckIostatOpenClose( &!
        iostat = iostat,                        &!
        silent = buf_silent,                    &!
        mode   = mode_CheckIostatOpenClose_OPEN &!
      )

    end if

  end subroutine



  ! check <iostat> of <READ> and <WRITE> statement
  subroutine CheckIostatReadWrite( iostat, iomsg, silent, mode )

    ! arguments for this <subroutine>
    integer( kind=int32 ),      intent(in)           :: iostat
    character( len=*, kind=1 ), intent(in), optional :: iomsg
    logical,                    intent(in)           :: silent
    integer( kind=int32 ),      intent(in)           :: mode

    ! local variables for this <subroutine>
    character( len=5, kind=1 ) :: name_statement_buf

    select case( mode )
      case( mode_CheckIostatReadWrite_READ );  name_statement_buf = name_statement_READ
      case( mode_CheckIostatReadWrite_WRITE ); name_statement_buf = name_statement_WRITE
    end select

    select case( iostat )

      ! TRUE_END : when it have succeeded
      case( 0_int32 )

        if( .not. silent ) then

          call PrintOnConsoleStatement( trim( name_statement_buf ) )
          call PrintOnConsoleStatus
          write( unit= output_unit, fmt= '(A,1X,A,1X,A,/)', advance= 'yes' ) &!
            str_ItHasSucceededTo,     &!
            trim( name_statement_buf ), &!
            'the target.'

        end if

        return  ! TRUE_END

      ! BAD_END : when it have failed
      case default

        call PrintOnConsoleStatement( trim( name_statement_buf ) )
        call PrintOnConsoleStatus

        write( unit= output_unit, fmt= '(A,A,A)', advance= 'yes' ) &!
          'An error was detected in <', trim( name_statement_buf ), '>.'

        write( unit= output_unit, fmt= '(A,1X,I0)', advance= 'yes' ) &!
          str_TheValueOfIostatIs, iostat

        if( present(iomsg) ) then
          call PrintOnConsoleErrMsg
          write( unit=output_unit, fmt='(A,/)', advance='yes' ) trim(iomsg)
        end if

        call StopWithMessage  ! BAD_END
        
    end select

  end subroutine CheckIostatReadWrite


  ! check <iostat> of <READ> statement
  subroutine CheckIostatRead( iostat, iomsg, silent )

    ! arguments for this <subroutine>
    integer( kind=int32 ),      intent(in)           :: iostat
    character( len=*, kind=1 ), intent(in), optional :: iomsg
    logical,                    intent(in), optional :: silent

    ! local variable for this <subroutine>
    logical :: buf_silent

    if( .not. present( silent ) ) then
      buf_silent = .false.
    else
      buf_silent = silent
    end if

    if( present( iomsg ) ) then

      call CheckIostatReadWrite( &!
        iostat = iostat,                        &!
        iomsg  = iomsg,                         &!
        silent = buf_silent,                    &!
        mode   = mode_CheckIostatReadWrite_READ &!
      )

    else

      call CheckIostatReadWrite( &!
        iostat = iostat,                        &!
        silent = buf_silent,                    &!
        mode   = mode_CheckIostatReadWrite_READ &!
      )

    end if

  end subroutine


  ! check <iostat> of <WRITE> statement
  subroutine CheckIostatWrite( iostat, iomsg, silent )

    ! arguments for this <subroutine>
    integer( kind=int32 ),      intent(in)           :: iostat
    character( len=*, kind=1 ), intent(in), optional :: iomsg
    logical,                    intent(in), optional :: silent

    ! local variable for this <subroutine>
    logical :: buf_silent

    if( .not. present( silent ) ) then
      buf_silent = .false.
    else
      buf_silent = silent
    end if

    if( present( iomsg ) ) then

      call CheckIostatReadWrite( &!
        iostat = iostat,                         &!
        iomsg  = iomsg,                          &!
        silent = buf_silent,                     &!
        mode   = mode_CheckIostatReadWrite_WRITE &!
      )

    else

      call CheckIostatReadWrite( &!
        iostat = iostat,                         &!
        silent = buf_silent,                     &!
        mode   = mode_CheckIostatReadWrite_WRITE &!
      )

    end if

  end subroutine


end module
!----------------------------------------------------------------------------------------------------------------------------------!
! End of this F08 file                                                                                                             !
!----------------------------------------------------------------------------------------------------------------------------------!