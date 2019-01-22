! ------------------------------------------------------------------------------------------------------------------------------- !
! [Project Euler] 0003                                                                                                            !
! https://projecteuler.net/problem=3                                                                                              !
! http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%203                                                        !
! ------------------------------------------------------------------------------------------------------------------------------- !
module Problem0003

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of <subroutine>s and <function>s in this <module>
  public  :: Problem0003_01       ! interface
  public  :: Problem0003_02       ! interface
  public  :: Problem0003_03       ! interface
  private :: Problem0003_01_INT32 ! function
  private :: Problem0003_01_INT64 ! function
  private :: Problem0003_02_INT32 ! function
  private :: Problem0003_02_INT64 ! function

  ! <interface>s for this <program>
  interface Problem0003_01
    module procedure Problem0003_01_INT32
    module procedure Problem0003_01_INT64
  end interface

  interface Problem0003_02
    module procedure Problem0003_02_INT32
    module procedure Problem0003_02_INT64
  end interface

  interface Problem0003_03
    module procedure Problem0003_03_INT32
    module procedure Problem0003_03_INT64
  end interface

  
  ! <subroutine>s and <function>s in this <module> is below
  contains

  pure function Problem0003_01_INT32( target ) result( last_factor )

    ! argument of this <function>
    integer( kind=INT32 ), intent(in) :: target

    ! return value of this <function>
    integer( kind=INT32 ) :: last_factor

    ! support variables for this <function>
    integer( kind=INT32 ) :: buffer
    integer( kind=INT32 ) :: factor


    ! STEP.01 !
    buffer      = target
    factor      = 2_INT32
    last_factor = 1_INT32

    ! STEP.02 !
    do while( buffer .gt. 1_INT32 )

      if( mod( buffer, factor ) .eq. 0_INT32 ) then
        last_factor = factor
        buffer      = buffer / factor
        do while( mod( buffer, factor ) .eq. 0_INT32 )
          buffer = buffer / factor
        end do
      end if

      factor = factor + 1_INT32

    end do

    ! STEP.END !
    return

  end function Problem0003_01_INT32

  pure function Problem0003_01_INT64( target ) result( last_factor )

    ! argument of this <function>
    integer( kind=INT64 ), intent(in) :: target

    ! return value of this <function>
    integer( kind=INT64 ) :: last_factor

    ! support variables for this <function>
    integer( kind=INT64 ) :: buffer
    integer( kind=INT64 ) :: factor


    ! STEP.01 !
    buffer      = target
    factor      = 2_INT64
    last_factor = 1_INT64

    ! STEP.02 !
    do while( buffer .gt. 1_INT64 )

      if( mod( buffer, factor ) .eq. 0_INT64 ) then
        last_factor = factor
        buffer      = buffer / factor
        do while( mod( buffer, factor ) .eq. 0_INT64 )
          buffer = buffer / factor
        end do
      end if

      factor = factor + 1_INT64

    end do

    ! STEP.END !
    return

  end function Problem0003_01_INT64



  pure function Problem0003_02_INT32( target ) result( last_factor )

    ! argument of this <function>
    integer( kind=INT32 ), intent(in) :: target

    ! return value of this <function>
    integer( kind=INT32 ) :: last_factor

    ! support variables for this <function>
    integer( kind=INT32 ) :: buffer
    integer( kind=INT32 ) :: factor


    ! STEP.01 !
    buffer = target
    factor = 2_INT32

    ! STEP.02 !
    if( mod( buffer, factor ) .eq. 0_INT32 ) then
      buffer      = buffer / factor
      last_factor = factor
      do while( mod( buffer, factor ) .eq. 0_INT32 )
        buffer = buffer / factor
      end do
    else
      last_factor = 1_INT32
    end if

    ! STEP.03 !
    factor = 3_INT32

    ! STEP.04 !
    do while( buffer .gt. 1_INT32 )

      if( mod( buffer, factor ) .eq. 0_INT32 ) then
        last_factor = factor
        buffer      = buffer / factor
        do while( mod( buffer, factor ) .eq. 0_INT32 )
          buffer = buffer / factor
        end do
      end if

      factor = factor + 2_INT32

    end do

    ! STEP.END !
    return

  end function Problem0003_02_INT32

  pure function Problem0003_02_INT64( target ) result( last_factor )

    ! argument of this <function>
    integer( kind=INT64 ), intent(in) :: target

    ! return value of this <function>
    integer( kind=INT64 ) :: last_factor

    ! support variables for this <function>
    integer( kind=INT64 ) :: buffer
    integer( kind=INT64 ) :: factor


    ! STEP.01 !
    buffer = target
    factor = 2_INT64

    ! STEP.02 !
    if( mod( buffer, factor ) .eq. 0_INT64 ) then
      buffer      = buffer / factor
      last_factor = factor
      do while( mod( buffer, factor ) .eq. 0_INT64 )
        buffer = buffer / factor
      end do
    else
      last_factor = 1_INT64
    end if

    ! STEP.03 !
    factor = 3_INT64

    ! STEP.04 !
    do while( buffer .gt. 1_INT64 )

      if( mod( buffer, factor ) .eq. 0_INT64 ) then
        last_factor = factor
        buffer      = buffer / factor
        do while( mod( buffer, factor ) .eq. 0_INT64 )
          buffer = buffer / factor
        end do
      end if

      factor = factor + 2_INT64

    end do

    ! STEP.END !
    return

  end function Problem0003_02_INT64



  pure function Problem0003_03_INT32( target ) result( last_factor )

    ! argument of this <function>
    integer( kind=INT32 ), intent(in) :: target

    ! return value of this <function>
    integer( kind=INT32 ) :: last_factor
    integer( kind=INT32 ) :: max_factor

    ! support variables for this <function>
    integer( kind=INT32 ) :: buffer
    integer( kind=INT32 ) :: factor


    ! STEP.01 !
    buffer = target

    ! STEP.02 !
    if( mod( buffer, 2_INT32 ) .eq. 0_INT32 ) then
      buffer      = buffer / 2_INT32
      last_factor = 2_INT32
      do while( mod( buffer, 2_INT32 ) .eq. 0_INT32 )
        buffer = buffer / 2_INT32
      end do
    else
      last_factor = 1_INT32
    end if

    ! STEP.03 !
    factor     = 3_INT32
    max_factor = nint( sqrt( real( buffer, kind=REAL64 ) ), kind=INT32 )

    ! STEP.04 !
    do while( buffer .gt. 1_INT32 .and. factor .le. max_factor )

      if( mod( buffer, factor ) .eq. 0_INT32 ) then
        buffer      = buffer / factor
        last_factor = factor
        do while( mod( buffer, factor ) .eq. 0_INT32 )
          buffer = buffer / factor
        end do
        max_factor = nint( sqrt( real( buffer, kind=REAL64 ) ), kind=INT32 )
      end if

      factor = factor + 2_INT32

    end do

    ! STEP.05 !
    if( buffer .ne. 1_INT32 ) then
      last_factor = buffer
    end if

    ! STEP.END !
    return

  end function Problem0003_03_INT32

  pure function Problem0003_03_INT64( target ) result( last_factor )

    ! argument of this <function>
    integer( kind=INT64 ), intent(in) :: target

    ! return value of this <function>
    integer( kind=INT64 ) :: last_factor
    integer( kind=INT64 ) :: max_factor

    ! support variables for this <function>
    integer( kind=INT64 ) :: buffer
    integer( kind=INT64 ) :: factor


    ! STEP.01 !
    buffer = target

    ! STEP.02 !
    if( mod( buffer, 2_INT64 ) .eq. 0_INT64 ) then
      buffer      = buffer / 2_INT64
      last_factor = 2_INT64
      do while( mod( buffer, 2_INT64 ) .eq. 0_INT64 )
        buffer = buffer / 2_INT64
      end do
    else
      last_factor = 1_INT64
    end if

    ! STEP.03 !
    factor     = 3_INT64
    max_factor = nint( sqrt( real( buffer, kind=REAL64 ) ), kind=INT64 )

    ! STEP.04 !
    do while( buffer .gt. 1_INT64 .and. factor .le. max_factor )

      if( mod( buffer, factor ) .eq. 0_INT64 ) then
        buffer      = buffer / factor
        last_factor = factor
        do while( mod( buffer, factor ) .eq. 0_INT64 )
          buffer = buffer / factor
        end do
        max_factor = nint( sqrt( real( buffer, kind=REAL64 ) ), kind=INT64 )
      end if

      factor = factor + 2_INT64

    end do

    ! STEP.05 !
    if( buffer .ne. 1_INT64 ) then
      last_factor = buffer
    end if

    ! STEP.END !
    return

  end function Problem0003_03_INT64

end module Problem0003
! ------------------------------------------------------------------------------------------------------------------------------- !
! gfortran ^                                                                                                                      !
! -c ^                                                                                                                            !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! D:\GitHub\Fortran\ProjectEuler\Problem0003\Problem0003_01.f08 ^                                                                 !
! D:\GitHub\Fortran\ProjectEuler\Problem0003\main.f08                                                                             !
!                                                                                                                                 !
! gfortran ^                                                                                                                      !
! -o Problem0003_01.exe ^                                                                                                         !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! D:\gfortran\Problem0003_01.o ^                                                                                                  !
! D:\gfortran\main.o                                                                                                              !
! ------------------------------------------------------------------------------------------------------------------------------- !