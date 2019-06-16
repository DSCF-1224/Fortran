!----------------------------------------------------------------------------------------------------------------------------------!
! [target]                                                                                                                         !
! check return value of <STAT> or <STATVAL> from subroutine <ACCLOATE> and <DEALLOCATE>                                            !
! [reference]                                                                                                                      !
! https://qiita.com/bluepost59/items/ca560c49a8c19484db9d                                                                          !
!----------------------------------------------------------------------------------------------------------------------------------!

module support_allocation

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env ! gfortran 8.1.0
  use, non_intrinsic :: support_support ! version 2018.12.15

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of <subroutine>s and <function>s in this <module>
  private :: write_errmsg_mode
  private :: write_errmsg_stat
  private :: write_errmsg_errmsg
  private :: write_name_class
  private :: set_mode_status
  private :: get_mode_status
  private :: set_stat_status
  private :: get_stat_status
  private :: set_errmsg_status
  private :: get_errmsg_status
  private :: evaluate_status
  private :: write_name_statement_staus 

  ! constants for this <module>
  character(len=23 ,kind=1), parameter, private :: name_class_concerned      = '<class> status_allocate'
  character(len= 8 ,kind=1), parameter, private :: name_statement_allocate   = 'ALLOCATE'
  character(len=10 ,kind=1), parameter, private :: name_statement_deallocate = 'DE'//name_statement_allocate(:)

  ! <type> for this <module>
  type class_status_allocate
    logical                          , private :: HasSetMode
    logical                          , private :: HasGotStat
    logical                          , private :: HasGotMsg
    logical                          , private :: mode       ! true => subroutine <ALLOCATE>, false => subroutine <DEALLOCATE>
    integer  (INT32)                 , public  :: stat
    character(len=len_errmsg, kind=1), public  :: errmsg
  contains
    procedure :: set_mode             => set_mode_status
    procedure :: get_mode             => get_mode_status
    procedure :: set_stat             => set_stat_status
    procedure :: get_stat             => get_stat_status
    procedure :: set_errmsg           => set_errmsg_status
    procedure :: get_errmsg           => get_errmsg_status
    procedure :: evaulate             => evaluate_status
    procedure :: write_name_statement => write_name_statement_staus
  end type class_status_allocate

  ! declaration of constructor
  interface status_allocate
    module procedure :: initialize_class
  end interface status_allocate


  ! contained <subroutine>s and <function>s are below
  contains

  ! constructor of this <class>
  type(class_status_allocate) function initialize_class()
    initialize_class%HasSetMode = .false.
    initialize_class%HasGotStat = .false.
    initialize_class%HasGotMsg  = .false.
    return
  end function initialize_class

  ! set the property `mode`
  ! (show whether the target instance is used for <ALLOCATE> or <DEALLOCATE>)
  subroutine set_mode_status (self, bool)

    ! arguments for this <subroutine>
    class(class_status_allocate), intent(inout) :: self
    logical,                      intent(in)    :: bool

    self%mode       = bool
    self%HasSetMode = .true.
    return

  end subroutine

  ! get the property `mode`
  ! (show whether the target instance is used for <ALLOCATE> or <DEALLOCATE>)
  logical function get_mode_status (self)

    ! arguments for this <subroutine>
    class(class_status_allocate) :: self

    if ( .not. self%HasSetMode ) then
      get_mode_status = .false.
      call write_errmsg_mode(OUTPUT_UNIT)
    else
      get_mode_status = self%mode
    end if

    return

  end function get_mode_status

  ! set the property `status`
  subroutine set_stat_status (self, val)

    ! arguments for this <subroutine>
    class(class_status_allocate), intent(inout) :: self
    integer(INT32),               intent(in)    :: val

    self%stat       = val
    self%HasGotStat = .true.
    return
  
  end subroutine set_stat_status

  ! get the property `status`
  integer(INT32) function get_stat_status (self)

    ! arguments for this <function>
    class(class_status_allocate) :: self

    if ( .not. self%HasGotStat ) then
      get_stat_status = huge(1)
      call write_errmsg_stat(OUTPUT_UNIT)
    else
      get_stat_status = self%stat
    end if

    return
  
  end function get_stat_status

  ! set the property `errmsg`
  subroutine set_errmsg_status (self, msg)

    ! arguments for this <subroutine>
    class(class_status_allocate), intent(inout) :: self
    character(len=*, kind=1),     intent(in)    :: msg

    self%errmsg    = trim(msg)
    self%HasGotMsg = .true.
    return
  
  end subroutine set_errmsg_status

  ! get the property `errmsg`
  character(len=len_errmsg) function get_errmsg_status (self)

    ! arguments for this <function>
    class(class_status_allocate) :: self

    if ( .not. self%HasGotMsg ) then
      call write_errmsg_errmsg(OUTPUT_UNIT)
    else
      get_errmsg_status = self%errmsg
    end if

    return

  end function get_errmsg_status

  ! evvaluate the return value of statement <ALLOCATE> and <DEALLOCATE>
  subroutine evaluate_status (self, unit, silent)

    ! arguments for this <subroutine>
    class(class_status_allocate), intent(inout) :: self
    integer(INT32),               intent(in)    :: unit
    logical,                      intent(in)    :: silent

    if (.not. self%get_mode()) then
      call write_errmsg_mode(unit=OUTPUT_UNIT)
    else if (.not. self%HasGotStat) then
      call write_errmsg_stat(unit=OUTPUT_UNIT)
    else if (.not. self%HasGotMsg) then
      call write_errmsg_errmsg(unit=OUTPUT_UNIT)
    end if

    select case (self%get_stat())

      case (0_INT32) ! if it was succeeded to allocate/deallocate
        
        if (.not. silent) then

          call self%write_name_statement()
          write(unit=unit, fmt='(A,1X)',advance='no') 'It has succeeded to'

          if (self%get_mode()) then
            write(unit=unit, fmt='(A)',advance='no') name_statement_allocate
          else
            write(unit=unit, fmt='(A)',advance='no') name_statement_deallocate
          end if

          write(unit=unit, fmt='(1X,A)',advance='yes') 'the array.'

        end if

        return ! TRUE_END

      case default

        call self%write_name_statement()
        call PrintOnConsoleStatus

        write(unit=unit, fmt='(A,1X)', advance='no') 'An'

        select case (self%get_stat())
          case(1_int32:)
            write(unit=unit, fmt='(A,1X)', advance='no') 'unrecoverable'
          case(:-1_int32)
            write(unit=unit, fmt='(A,1X)', advance='no') 'undefined'
        end select

        write(unit=unit, fmt='(A)',       advance='yes' ) 'error was detected !'
        write(unit=unit, fmt='(A,1X,I8)', advance='yes' ) 'The value of <STAT> is', self%get_stat()
        
        call PrintOnConsoleErrMsg
        write(unit=unit, fmt='(A)', advance='yes') trim(self%errmsg)

        call StopWithMessage  ! BAD_END

    end select

  end subroutine evaluate_status

  ! output the name of target statement
  subroutine write_name_statement_staus (self)

    ! arguments for this <subroutine>
    class(class_status_allocate) :: self

    if (self%mode) then
      call PrintOnConsoleStatement (name_statement_allocate)
    else
      call PrintOnConsoleStatement (name_statement_deallocate)
    end if

    return

  end subroutine write_name_statement_staus

  ! output the name of this class
  subroutine write_name_class (unit)

    ! arguments for this <subroutine>
    integer(INT32), intent(in) :: unit

    write(unit=unit, fmt='(A)', advance='yes') name_class_concerned(:)
    return

  end subroutine write_name_class

  ! output the error message about the property `mode`
  subroutine write_errmsg_mode (unit)

    ! arguments for this <subroutine>
    integer(INT32), intent(in) :: unit

    call write_name_class (unit=unit)
    write(unit=unit, fmt='(A)', advance='yes') 'The property `mode` has not been specified yet.'
    call StopWithMessage
    return

  end subroutine write_errmsg_mode

  ! output the error message about the property `stat`
  subroutine write_errmsg_stat (unit)

    ! arguments for this <subroutine>
    integer(INT32), intent(in) :: unit

    call write_name_class (unit=unit)
    write(unit=unit, fmt='(A)', advance='yes') 'The property `stat` has not been specified yet.'
    call StopWithMessage
    return

  end subroutine write_errmsg_stat

  ! output the error message about the property `errmsg`
  subroutine write_errmsg_errmsg (unit)

    ! arguments for this <subroutine>
    integer(INT32), intent(in) :: unit

    call write_name_class (unit=unit)
    write(unit=unit, fmt='(A)', advance='yes') 'The property `errmsg` has not been specified yet.'
    call StopWithMessage
    return

  end subroutine write_errmsg_errmsg

end module support_allocation

!----------------------------------------------------------------------------------------------------------------------------------!
! End of this F08 file                                                                                                             !
!----------------------------------------------------------------------------------------------------------------------------------!
