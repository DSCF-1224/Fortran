! ------------------------------------------------------------------------------------------------------------------------------- !
! [Project Euler] 0001                                                                                                            !
! https://projecteuler.net/problem=1                                                                                              !
! http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%201                                                        !
! ------------------------------------------------------------------------------------------------------------------------------- !
module Problem0001

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! accessibility of <subroutine>s and <function>s in this <module>
  public  :: Problem0001_01
  public  :: Problem0001_02
  public  :: Problem0001_03
  private :: Problem0001_03_sub
  
  ! <subroutine>s and <function>s in this <module> is below
  contains

  pure function Problem0001_01( limit ) result( sum )

    ! argument of this <function>
    integer( kind=INT64 ), intent(in) :: limit

    ! return value of this <function>
    integer( kind=INT64 ) :: sum

    ! support variables for this <function>
    integer( kind=INT64 ) :: itr


    ! STEP.01 !
    itr = 1_INT64
    sum = 0_INT64

    ! STEP.02 !
    do while( itr .lt. limit )
      if( mod( itr, 3_INT64 ) .eq. 0_INT64 .or. mod( itr, 5_INT64 ) .eq. 0_INT64 ) sum = sum + itr
      itr = itr + 1_INT64
    end do

    ! STEP.END !
    return

  end function Problem0001_01

  pure function Problem0001_02( limit ) result( sum )

    ! argument of this <function>
    integer( kind=INT64 ), intent(in) :: limit

    ! return value of this <function>
    integer( kind=INT64 ) :: sum

    ! support variables for this <function>
    integer( kind=INT64 ) :: itr


    ! STEP.01 !
    sum = 0_INT64

    ! STEP.02 !
    do itr = 1, limit-1, 1
      if( &!
        mod( itr, 3_INT64 ) .eq. 0_INT64 .or. &!
        mod( itr, 5_INT64 ) .eq. 0_INT64      &!
      ) sum = sum + itr
    end do

    ! STEP.END !
    return

  end function Problem0001_02

  pure function Problem0001_03_sub( divisor, limit ) result( sum )

    ! argument of this <function>
    integer( kind=INT64 ), intent(in) :: divisor
    integer( kind=INT64 ), intent(in) :: limit

    ! return value of this <function>
    integer( kind=INT64 ) :: sum

    ! support variables for this <function>
    integer( kind=INT64 ) :: num_terms


    ! STEP.01 !
    num_terms = ( limit - 1_INT64 ) / divisor

    ! STEP.02 !
    sum = divisor * num_terms * ( num_terms + 1_INT64 ) / 2_INT64

    ! STEP.END !
    return

  end function Problem0001_03_sub

  pure function Problem0001_03( limit ) result( sum )

    ! argument of this <function>
    integer( kind=INT64 ), intent(in) :: limit

    ! return value of this <function>
    integer( kind=INT64 ) :: sum


    ! STEP.01 !
    sum =       Problem0001_03_sub( divisor= 3_INT64, limit=limit )
    sum = sum + Problem0001_03_sub( divisor= 5_INT64, limit=limit )
    sum = sum - Problem0001_03_sub( divisor=15_INT64, limit=limit )

    ! STEP.END !
    return

  end function Problem0001_03

end module Problem0001
! ------------------------------------------------------------------------------------------------------------------------------- !
! gfortran ^                                                                                                                      !
! -c ^                                                                                                                            !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! D:\GitHub\Fortran\ProjectEuler\Problem0001\Problem0001_01.f08 ^                                                                 !
! D:\GitHub\Fortran\ProjectEuler\Problem0001\main.f08                                                                             !
!                                                                                                                                 !
! gfortran ^                                                                                                                      !
! -o Problem0001_01.exe ^                                                                                                         !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! D:\gfortran\Problem0001_01.o ^                                                                                                  !
! D:\gfortran\main.o                                                                                                              !
! ------------------------------------------------------------------------------------------------------------------------------- !