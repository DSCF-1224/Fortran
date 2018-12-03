!----------------------------------------------------------------------!
! [target]                                                             !
! check return value of <STAT> or <STATVAL>                            !
!----------------------------------------------------------------------!
module support_io

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env ! gfortran 8.1.0
  use, non_intrinsic :: support_support

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of <subroutine>s and <function>s in this <module>
  public :: CheckStatAllocate
  public :: CheckStatDeallocate

  ! variables for this <module>
  character( len=len_ErrMsg ), public :: buf_ErrMsg_io


  ! <subroutine>s and <function>s for this <module>
  contains


  ! check <stat> of <allocate> statement
  subroutine CheckStatAllocate( stat, errmsg )

    ! argument for this <subroutine>
    integer(kind=int32),                 intent(in)           :: stat
    character( len=len_ErrMsg, kind=1 ), intent(in), optional :: errmsg

    call PrintOnConsoleStatement( "ALLOCATE" )
    call PrintOnConsoleStatus

    select case( stat )
      case(0)
        print '(A/)', 'It have succeeded to allocate the array.'
        return
        ! TRUE_END
      case default
        select case( stat )
          case(1:)
            print '(A$)', 'An unrecoverable error'
          case(:-1)
            print '(A$)', 'An undefined error'
        end select

        print '(A)', 'was detected !'
        print '(A,I8)', 'stat value is ', stat
        
        if( present(errmsg) ) then
          call PrintOnConsoleErrMsg
          print '(A/)', trim( errmsg )
        end if
        
        call WaitEnter
        call StopWithMessage
        ! BAD_END
    end select

  end subroutine


  ! check <stat> of <allocate> statement
  subroutine CheckStatDeallocate( stat, errmsg )

    ! argument for this <subroutine>
    integer(kind=int32),                 intent(in)           :: stat
    character( len=len_ErrMsg, kind=1 ), intent(in), optional :: errmsg

    call PrintOnConsoleStatement( "DEALLOCATE" )
    call PrintOnConsoleStatus

    select case( stat )
      case(0)
        print '(A/)', 'it have succeeded to deallocate the array.'
        return
        ! TRUE_END
      case default
        select case( stat )
          case(1:)
            print '(A$)', 'An unrecoverable error'
          case(:-1)
            print '(A$)', 'An undefined error'
        end select

        print '(A)', 'was detected !'
        print '(A,I8)', 'stat value is ', stat
        
        if( present(errmsg) ) then
          call PrintOnConsoleErrMsg
          print '(A/)', trim( errmsg )
        end if
        
        call WaitEnter
        call StopWithMessage
        ! BAD_END
    end select

  end subroutine


  ! check <iostat> of <CLOSE> statement
  subroutine CheckIostatClose( iostat, iomsg )

    ! argument for this <subroutine>
    integer( kind=int32 ),               intent(in)           :: iostat
    character( len=len_ErrMsg, kind=1 ), intent(in), optional :: iomsg


    call PrintOnConsoleStatement( 'CLOSE' )
    call PrintOnConsoleStatus

    select case( iostat )
      case( 0_int32 )
        print '(A/)', 'It have succeeded to close the target file.'
        return
        ! TRUE_END
      case default

        print '(A,1X$)',   'An error was detected.'
        print '(A,1X,I8)', '<IOSTAT> value is', iostat

        if( present(iomsg) ) then
          call PrintOnConsoleErrMsg
          print '(A)', trim( iomsg )
        end if

        call WaitEnter
        call StopWithMessage
        ! BAD_END

    end select

  end subroutine


  ! check <iostat> of <OPEN> statement
  subroutine CheckIostatOpen( iostat, iomsg )

    ! argument for this <subroutine>
    integer( kind=int32 ),               intent(in)           :: iostat
    character( len=len_ErrMsg, kind=1 ), intent(in), optional :: iomsg


    call PrintOnConsoleStatement( 'OPEN' )
    call PrintOnConsoleStatus

    select case( iostat )
      case( 0_int32 )
        print '(A/)', 'It have succeeded to open the target file.'
        return
        ! TRUE_END
      case default

        print '(A,1X$)',   'An error was detected.'
        print '(A,1X,I8)', '<IOSTAT> value is', iostat

        if( present(iomsg) ) then
          call PrintOnConsoleErrMsg
          print '(A)', trim( iomsg )
        end if

        call WaitEnter
        call StopWithMessage
        ! BAD_END

    end select

  end subroutine

end module