!----------------------------------------------------------------------!
! [target]                                                             !
! mini parts for support                                               !
!----------------------------------------------------------------------!
module support_support

  ! <module>s to import
  use, intrinsic :: iso_fortran_env ! gfortran 8.1.0

  ! require all variables to be explicitly declared
  implicit none

  integer, parameter, public :: len_ErrMsg = 512

  ! accessibility of <subroutine>s and <function>s in this <module>
  public :: JointPath
  public :: PrintOnConsoleError
  public :: PrintOnConsoleErrMsg
  public :: PrintOnConsoleStatus
  public :: PrintOnConsoleStatement
  public :: PrintOnConsoleStatementName
  public :: ReachedTheEnd
  public :: WaitEnter

  ! <subroutine>s and <function>s for this <module>
  contains


  ! only print "[Statement]" on console
  subroutine PrintOnConsoleStatementName( name )

    ! argument for this <subroutine>
    character( len=* ), intent(in) :: name

    print '(A,1X,A9)', name, 'Statement'
    return

  end subroutine


  ! only print "[Error]" on console
  subroutine PrintOnConsoleStatement( name )

    ! argument for this <subroutine>
    character( len=* ), intent(in) :: name

    print '(A11/,A)', '[Statement]', name
    return
  
  end subroutine


  ! only print "[Error]" on console
  subroutine PrintOnConsoleError

    print '(A7)', '[Error]'
    return
  
  end subroutine


  ! only print "[Error Message]" on console
  subroutine PrintOnConsoleErrMsg

    print '(A15)', '[Error Message]'
    return
  
  end subroutine


  ! only print "[Status]" on console
  subroutine PrintOnConsoleStatus

    print '(A8)', '[Status]'
    return

  end subroutine


  ! joint `folder path` and `file name` to use as path
  pure function JointPath( parent, child )

    ! arguments for this <function>
    character( len=* ), intent(in) :: parent
    character( len=* ), intent(in) :: child

    ! return value of this <function>
    character( len=len(parent)+len(child) ) :: JointPath

    ! local variables for this <function>
    integer(kind=int32) :: len_str_parent

    len_str_parent = len_trim( adjustl( parent ) )

    if( parent(len_str_parent:len_str_parent) .eq. '\' ) then
      JointPath = &!
        trim( adjustl(parent) ) // &!
        trim( adjustl(child)  )
      return
    else
      JointPath = &
        trim( adjustl(parent) ) // '\' // &!
        trim( adjustl(child)  )
      return
    end if

  end function


  ! <STOP> statement with simple Error message
  subroutine StopWithMessage
    stop '<STOP> statement was activated !'
  end subroutine


  ! temporary stop waiting input from keyboard
  ! http://www.nag-j.co.jp/fortran/tips/tips_WaitEnter.html#_WaitEnter
  subroutine WaitEnter

    print '(A/,A)',           &!
      '[TEMPORARY STOP]',    &!
      'Please press Enter:'
    read *
    return

  end subroutine

  subroutine ReachedTheEnd

    call PrintOnConsoleStatus
    print '(A)', "All processes have finished successfully."
    print '(A)', "press any key to end this process."
    read *
    return

  end subroutine

end module