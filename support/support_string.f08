!----------------------------------------------------------------------------------------------------------------------------------!
! [target]                                                                                                                         !
! support the process which uses character constant/variable                                                                       !
!----------------------------------------------------------------------------------------------------------------------------------!
module support_string

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env ! gfortran 8.1.0
  use, non_intrinsic :: support_support ! version 2018.12.15
  use, non_intrinsic :: support_io      ! version 2018.12.15


  ! require all variables to be explicitly declared
  implicit none


  ! constants in this <module>
  integer( kind=int32 ), parameter, private :: limit_digit = 10
  integer( kind=int32 ), parameter, private :: limit_val   = 10_int32 ** limit_digit - 1


  ! accessibility of <subroutine>s and <function>s in this <module>
  public  :: CStrFillZero_Int
  private :: CStrFillZero_Int32


  ! <interface> for this <module>
  interface CStrFillZero_Int
    module procedure :: CStrFillZero_Int32
  end interface CStrFillZero_Int


  ! <subroutine>s and <function>s for this <module>
  contains


  ! convert int32 -> character
  function CStrFillZero_Int32( val, digit ) result( str )

    ! arguments for this <function>
    integer( kind=int32 ),          intent(in) :: val
    integer( kind=int32 ),          intent(in) :: digit

    ! return value of this <function>
    character( len=digit, kind=1 ) :: str

    ! local support variable
    character( len=limit_digit, kind=1 ) :: str_digit
    character(:), allocatable            :: str_fmt
    integer( kind=int32 )                :: iostat

    ! STEP.01
    ! check the value to convert to string
    if( val .lt. 0_int32 ) then

      call PrintOnConsoleStatement( "CStrFillZero_Int" )
      call PrintOnConsoleStatus
      write( unit=output_unit, fmt='(A,1X,I0,1X,A,/)', advance='yes' ) &!
        'Given target number :', &!
        val, &!
        'was too small to convert to string'

      call StopWithMessage ! BAD_END

    end if

    if( val .gt. limit_val ) then

      call PrintOnConsoleStatement( "CStrFillZero_Int" )
      call PrintOnConsoleStatus
      write( unit=output_unit, fmt='(A,1X,I0,1X,A,/)', advance='yes' ) &!
        'Given digit :', &!
        digit, &!
        'was too small to convert to string'

      call StopWithMessage ! BAD_END

    end if

    if( digit .gt. limit_digit ) then

      call PrintOnConsoleStatement( "CStrFillZero_Int" )
      call PrintOnConsoleStatus
      write( unit=output_unit, fmt='(A,1X,I0,1X,A,1X,I0/)', advance='yes' ) &!
        'Given digit :', &!
        digit, &!
        'is bigger than the limit of the number of digits:', &!
        limit_val

      call StopWithMessage ! BAD_END

    end if


    ! STEP.02
    ! make the string which represents the format
    write( unit=str_digit, fmt='(I3)', iostat= iostat, iomsg= buf_ErrMsg_io  ) digit
    str_fmt = "(I"//trim(adjustl(str_digit))//"."//trim(adjustl(str_digit))//")"

    ! ! STEP.03
    ! convert the value to string filling zero
    write( unit=str, fmt= str_fmt, iostat= iostat, iomsg= buf_ErrMsg_io ) val
    if( iostat .ne. 0_int32 ) then
      
      call PrintOnConsoleStatement( 'CStrFillZero_Int' )
      call PrintOnConsoleStatus
      write( unit=output_unit, fmt='(A,1X)', advance='no' ) 'failed to convert the value to string at'
      call PrintOnConsoleStatementName( 'WRITE' )
      call CheckIostatWrite( iostat= iostat, iomsg= buf_ErrMsg_io )

    end if

    return ! TRUE_END

  end function CStrFillZero_Int32


end module support_string
!----------------------------------------------------------------------------------------------------------------------------------!
! End of this F08 file                                                                                                             !
!----------------------------------------------------------------------------------------------------------------------------------!