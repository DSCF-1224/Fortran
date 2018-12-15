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

  ! variables for this <module>
  character( len=len_ErrMsg ), public :: buf_ErrMsg_io


  ! <subroutine>s and <function>s for this <module>
  contains


  ! check <stat> of <allocate> statement
  subroutine CheckStatAllocate( stat, errmsg, silent )

    ! arguments for this <subroutine>
    integer(kind=int32),                 intent(in)           :: stat
    character( len=len_ErrMsg, kind=1 ), intent(in), optional :: errmsg
    logical,                             intent(in), optional :: silent

    select case( stat )

      case( 0_int32 )

        if( present( silent ) .and. .not. silent ) then
          call PrintOnConsoleStatement( "ALLOCATE" )
          call PrintOnConsoleStatus
          write( unit=output_unit, fmt='(A)', advance='yes' ) 'It have succeeded to allocate the array.'
        end if

        return  ! TRUE_END

      case default

        call PrintOnConsoleStatement( "ALLOCATE" )
        call PrintOnConsoleStatus

        write( unit=output_unit, fmt='(A,1X)', advance='no' ) 'An'

        select case( stat )
          case(1_int32:)
            write( unit=output_unit, fmt='(A,1X)', advance='no' ) 'unrecoverable'
          case(:-1_int32)
            write( unit=output_unit, fmt='(A,1X)', advance='no' ) 'undefined'
        end select

        write( unit=output_unit, fmt='(A)',       advance='yes' ) 'error was detected !'
        write( unit=output_unit, fmt='(A,1X,I8)', advance='yes' ) 'stat value is', stat
        
        if( present(errmsg) ) then
          call PrintOnConsoleErrMsg
          write( unit=output_unit, fmt='(A)', advance='yes' ) trim(errmsg)
        end if
        
        call StopWithMessage  ! BAD_END

    end select

  end subroutine


  ! check <stat> of <allocate> statement
  subroutine CheckStatDeallocate( stat, errmsg, silent )

    ! arguments for this <subroutine>
    integer(kind=int32),                 intent(in)           :: stat
    character( len=len_ErrMsg, kind=1 ), intent(in), optional :: errmsg
    logical,                             intent(in), optional :: silent

    select case( stat )

      case( 0_int32 )

        if( present( silent ) .and. .not. silent ) then
          call PrintOnConsoleStatement( "DEALLOCATE" )
          call PrintOnConsoleStatus
          write( unit=output_unit, fmt='(A)', advance='yes' ) 'It have succeeded to deallocate the array.'
        end if

        return  ! TRUE_END

      case default

        call PrintOnConsoleStatement( "DEALLOCATE" )
        call PrintOnConsoleStatus

        write( unit=output_unit, fmt='(A,1X)', advance='no' ) 'An'

        select case( stat )
          case(1_int32:)
            write( unit=output_unit, fmt='(A,1X)', advance='no' ) 'unrecoverable'
          case(:-1_int32)
            write( unit=output_unit, fmt='(A,1X)', advance='no' ) 'undefined'
        end select

        write( unit=output_unit, fmt='(A)',       advance='yes' ) 'error was detected !'
        write( unit=output_unit, fmt='(A,1X,I8)', advance='yes' ) 'stat value is', stat
        
        if( present(errmsg) ) then
          call PrintOnConsoleErrMsg
          write( unit=output_unit, fmt='(A)', advance='yes' ) trim(errmsg)
        end if
        
        call StopWithMessage  ! BAD_END

    end select

  end subroutine


  ! check <iostat> of <CLOSE> statement
  subroutine CheckIostatClose( iostat, iomsg, silent )

    ! arguments for this <subroutine>
    integer( kind=int32 ),               intent(in)           :: iostat
    character( len=len_ErrMsg, kind=1 ), intent(in), optional :: iomsg
    logical,                             intent(in), optional :: silent


    select case( iostat )

      case( 0_int32 )

        if( present( silent ) .and. .not. silent ) then
          call PrintOnConsoleStatement( 'CLOSE' )
          call PrintOnConsoleStatus
          write( unit=output_unit, fmt='(A,/)' ) 'It have succeeded to close the target file.'
        end if

        return ! TRUE_END

      case default

        call PrintOnConsoleStatement( 'CLOSE' )
        call PrintOnConsoleStatus

        write( unit=output_unit, fmt='(A,1X)',    advance='no'  ) 'An error was detected.'
        write( unit=output_unit, fmt='(A,1X,I8)', advance='yes' ) '<IOSTAT> value is', iostat

        if( present(iomsg) ) then
          call PrintOnConsoleErrMsg
          write( unit=output_unit, fmt='(A,/)', advance='yes' ) trim(iomsg)
        end if

        call StopWithMessage  ! BAD_END

    end select

  end subroutine


  ! check <iostat> of <OPEN> statement
  subroutine CheckIostatOpen( iostat, iomsg, silent )

    ! arguments for this <subroutine>
    integer( kind=int32 ),               intent(in)           :: iostat
    character( len=len_ErrMsg, kind=1 ), intent(in), optional :: iomsg
    logical,                             intent(in), optional :: silent


    select case( iostat )

      case( 0_int32 )

        if( present( silent ) .and. .not. silent ) then
          call PrintOnConsoleStatement( 'OPEN' )
          call PrintOnConsoleStatus
          write( unit=output_unit, fmt='(A,/)') 'It have succeeded to open the target file.'
        end if

        return ! TRUE_END

      case default

        call PrintOnConsoleStatement( 'OPEN' )
        call PrintOnConsoleStatus

        write( unit=output_unit, fmt='(A,1X)',    advance='no'  ) 'An error was detected.'
        write( unit=output_unit, fmt='(A,1X,I8)', advance='yes' ) '<IOSTAT> value is', iostat

        if( present(iomsg) ) then
          call PrintOnConsoleErrMsg
          write( unit=output_unit, fmt='(A,/)', advance='yes' ) trim(iomsg)
        end if

        call StopWithMessage  ! BAD_END

    end select

  end subroutine


  ! check <iostat> of <READ> statement
  subroutine CheckIostatRead( iostat, iomsg, silent )

    ! arguments for this <subroutine>
    integer( kind=int32 ),               intent(in)           :: iostat
    character( len=len_ErrMsg, kind=1 ), intent(in), optional :: iomsg
    logical,                             intent(in), optional :: silent

    select case( iostat )
      case( 0_int32 )

        if( present( silent ) .and. .not. silent ) then
          call PrintOnConsoleStatement( 'READ' )
          call PrintOnConsoleStatus
          write( unit=output_unit, fmt='(A,/)') 'It have succeeded to read the target.'
        end if
        return
        ! TRUE_END

      case default

        call PrintOnConsoleStatement( 'READ' )
        call PrintOnConsoleStatus
        write( unit=output_unit, fmt='(A,1X)'    ) 'An error was detected.'
        write( unit=output_unit, fmt='(A,1X,I8)' ) '<IOSTAT> value is', iostat

        if( present(iomsg) ) then
          call PrintOnConsoleErrMsg
          write( unit=output_unit, fmt='(A,/)', advance='yes' ) trim(iomsg)
        end if

        call StopWithMessage  ! BAD_END

    end select

  end subroutine


  ! check <iostat> of <WRITE> statement
  subroutine CheckIostatWrite( iostat, iomsg, silent )

    ! arguments for this <subroutine>
    integer( kind=int32 ),               intent(in)           :: iostat
    character( len=len_ErrMsg, kind=1 ), intent(in), optional :: iomsg
    logical,                             intent(in), optional :: silent

    select case( iostat )
      case( 0_int32 )

        if( present( silent ) .and. .not. silent ) then
          call PrintOnConsoleStatement( 'WRITE' )
          call PrintOnConsoleStatus
          write( unit=output_unit, fmt='(A,/)') 'It have succeeded to write the target.'
        end if

        return
        ! TRUE_END

      case default

        call PrintOnConsoleStatement( 'WRITE' )
        call PrintOnConsoleStatus
        write( unit=output_unit, fmt='(A,1X)'    ) 'An error was detected.'
        write( unit=output_unit, fmt='(A,1X,I8)' ) '<IOSTAT> value is', iostat

        if( present(iomsg) ) then
          call PrintOnConsoleErrMsg
          write( unit=output_unit, fmt='(A,/)', advance='yes' ) trim(iomsg)
        end if

        call StopWithMessage  ! BAD_END

    end select

  end subroutine

end module
!----------------------------------------------------------------------------------------------------------------------------------!
! End of this F08 file                                                                                                             !
!----------------------------------------------------------------------------------------------------------------------------------!