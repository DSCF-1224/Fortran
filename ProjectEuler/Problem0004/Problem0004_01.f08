! ------------------------------------------------------------------------------------------------------------------------------- !
! [Project Euler] 0004                                                                                                            !
! https://projecteuler.net/problem=4                                                                                              !
! http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%204                                                        !
! ------------------------------------------------------------------------------------------------------------------------------- !
module Problem0004

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of <subroutine>s and <function>s in this <module>
  public  :: Problem0004_01      ! subroutine
  private :: Get_Digits          ! function
  private :: Check_IsPalindromic ! function
  private :: Pick_ValTargetDigit ! function

  
  ! <subroutine>s and <function>s in this <module> is below
  contains

  subroutine Problem0004_01( digit )

    ! argument of this <subroutine>
    integer( kind=INT64 ), intent(in) :: digit

    ! support variables for this <subroutine>
    integer( kind=INT64 ) :: product, buf_Palindromic
    integer( kind=INT64 ) :: itr_Max, itr_Min
    integer( kind=INT64 ) :: itrLv1, itrLv2
    logical               :: IsPalindromic, IsUpdated


    ! STEP.01 !
    itr_Max         = 10_INT64 **   digit - 1_INT64
    itr_Min         = 10_INT64 ** ( digit - 1_INT64 )
    product         =  0_INT64
    buf_Palindromic =  0_INT64

    ! STEP.02 !
    write( unit=output_unit, fmt='(60("-"))', advance='yes' )
    write( unit=output_unit, fmt='(2(3X,"factor",I1),A20,A12,A8)', advance='yes' ) 1, 2, 'product', 'palindromic', 'updated'

    do itrLv1 = itr_Max, itr_Min, -1_INT64
    do itrLv2 = itrLv1,  itr_Min, -1_INT64
      
      product = itrLv1 * itrLv2

      if( product .lt. buf_Palindromic ) then
        IsUpdated = .false.
        exit
      else
        IsPalindromic = Check_IsPalindromic( product )
        if( IsPalindromic ) then
          buf_Palindromic = product
          IsUpdated       = .true.
        end if
      end if

      if( IsPalindromic .and. IsUpdated ) then
        write( unit=output_unit, fmt='(2I10,I20,L12,L8)', advance='yes' ) itrLv1, itrLv2, product, IsPalindromic, IsUpdated
      end if

    end do
    end do

    ! STEP.03 !
    write( unit=output_unit, fmt='(A,I20)' ) 'result:', buf_Palindromic

    ! STEP.END !
    return

  end subroutine Problem0004_01


  pure function Get_Digits( target ) result( digits )

    ! argument of this <function>
    integer( kind=INT64 ), intent(in) :: target

    ! return value of this <function>
    integer( kind=INT64 ) :: digits

    digits = int( aint( log10( real( target, kind=REAL64 ) ), kind=REAL64 ), kind=INT64 ) + 1_INT64
    return

  end function Get_Digits


  pure function Check_IsPalindromic( target ) result( IsPalindromic )

    ! argument of this <function>
    integer( kind=INT64 ), intent(in) :: target

    ! return value of this <function>
    logical :: IsPalindromic

    ! support variables for this <function>
    integer( kind=INT64 ) :: num_digit
    integer( kind=INT64 ) :: buf1, buf2
    integer( kind=INT64 ) :: itr_digit, itr_Max


    ! STEP.01 !
    num_digit = Get_Digits( target )

    ! STEP.02 !
    select case ( mod( num_digit, 2_INT64 ) )
      case( 0_INT64 ); itr_Max =  num_digit          / 2_INT64
      case( 1_INT64 ); itr_Max = (num_digit-1_INT64) / 2_INT64
    end select

    do itr_digit = 1_INT64, itr_Max, 1_INT64

      buf1 = Pick_ValTargetDigit( target=target, digit=itr_digit                   )
      buf2 = Pick_ValTargetDigit( target=target, digit=num_digit-itr_digit+1_INT64 )

      if( buf1 .eq. buf2 ) then
        IsPalindromic = .true.
      else
        IsPalindromic = .false.; return
      end if

    end do

  end function Check_IsPalindromic


  pure function Pick_ValTargetDigit( target, digit ) result( num )

    ! argument of this <function>
    integer( kind=INT64 ), intent(in) :: target
    integer( kind=INT64 ), intent(in) :: digit

    ! return value of this <function>
    integer( kind=INT64 ) :: num

    num = mod( target, 10_INT64 ** digit )
    num = num    / ( 10_INT64 ** ( digit-1_INT64 ) )
    return

  end function Pick_ValTargetDigit

end module Problem0004
! ------------------------------------------------------------------------------------------------------------------------------- !
! gfortran ^                                                                                                                      !
! -c ^                                                                                                                            !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! D:\GitHub\Fortran\ProjectEuler\Problem0004\Problem0004_01.f08 ^                                                                 !
! D:\GitHub\Fortran\ProjectEuler\Problem0004\main.f08                                                                             !
!                                                                                                                                 !
! gfortran ^                                                                                                                      !
! -o Problem0004_01.exe ^                                                                                                         !
! -Wall -pedantic -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace ^                                !
! D:\gfortran\Problem0004_01.o ^                                                                                                  !
! D:\gfortran\main.o                                                                                                              !
! ------------------------------------------------------------------------------------------------------------------------------- !