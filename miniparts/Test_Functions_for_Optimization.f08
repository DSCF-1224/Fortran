! [reference]
! Test functions for optimization - Wikipedia
! https://en.wikipedia.org/wiki/Test_functions_for_optimization

module mod_Test_Functions_for_Optimization

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of <subroutine>s and <function>s in this <module>
  public :: Ackley_Function
  public :: Beale_Function
  public :: Goldstein_Price_Function
  public :: Rastrigin_Function
  public :: Rosenbrock_Function

  private :: Goldstein_Price_Function_REAL64, Goldstein_Price_Function_REAL128
  private :: Rastrigin_Function_REAL64,       Rastrigin_Function_REAL128
  private :: Rosenbrock_Function_REAL64,      Rosenbrock_Function_REAL128

  ! constants for this <module>
  real(kind=REAL64),  parameter, private :: CircCnst_REAL64        = acos(-1.0e+00_REAL64 )
  real(kind=REAL128), parameter, private :: CircCnst_REAL128       = acos(-1.0e+00_REAL128)
  real(kind=REAL64),  parameter, private :: CircCnst_REAL64_twice  = 2.0e+00_REAL64  * CircCnst_REAL64
  real(kind=REAL128), parameter, private :: CircCnst_REAL128_twice = 2.0e+00_REAL128 * CircCnst_REAL128

  real(kind=REAL64),  parameter, private :: NapierCnst_REAL64  = exp(1.0e+00_REAL64 )
  real(kind=REAL128), parameter, private :: NapierCnst_REAL128 = exp(1.0e+00_REAL128)
  

  ! <interface>s for this <module>
  interface Ackley_Function
    module procedure Ackley_Function_REAL64
    module procedure Ackley_Function_REAL128
  end interface Ackley_Function

  interface Beale_Function
    module procedure Beale_Function_REAL64
    module procedure Beale_Function_REAL128
  end interface Beale_Function

  interface Goldstein_Price_Function
    module procedure Goldstein_Price_Function_REAL64
    module procedure Goldstein_Price_Function_REAL128
  end interface Goldstein_Price_Function

  interface Rastrigin_Function
    module procedure Rastrigin_Function_REAL64
    module procedure Rastrigin_Function_REAL128
  end interface Rastrigin_Function

  interface Rosenbrock_Function
    module procedure Rosenbrock_Function_REAL64
    module procedure Rosenbrock_Function_REAL128
  end interface Rosenbrock_Function

  ! contained <subroutine>s and <function>s are below
  contains


  ! Ackley Function
  pure function Ackley_Function_REAL64 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL64), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL64) :: retval

    retval =&!
      - 2.0e+01_REAL64 * exp( -2.0e-01_REAL64 * sqrt( 5.0e-01_REAL64 * (x*x+y*y) ) ) &!
      - exp( 5.0e-01_REAL64 * ( cos(CircCnst_REAL64_twice*x) + cos(CircCnst_REAL64_twice*y) ) ) &!
      + NapierCnst_REAL64 &!
      + 2.0e+01_REAL64

    return

  end function Ackley_Function_REAL64

  pure function Ackley_Function_REAL128 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL128), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL128) :: retval

    retval =&!
      - 2.0e+01_REAL128 * exp( -2.0e-01_REAL128 * sqrt( 5.0e-01_REAL128 * (x*x+y*y) ) ) &!
      - exp( 5.0e-01_REAL128 * ( cos(CircCnst_REAL128_twice*x) + cos(CircCnst_REAL128_twice*y) ) ) &!
      + NapierCnst_REAL128 &!
      + 2.0e+01_REAL128

    return

  end function Ackley_Function_REAL128


  ! Beale Function
  pure function Beale_Function_REAL64 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL64), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL64) :: retval

    ! variables for this <function>
    real(kind=REAL64) :: factor(1:3)

    factor(1) = x * y
    factor(2) = y * factor(1)
    factor(3) = y * factor(2)

    factor(1) = 1.500e+00_REAL64 - x + factor(1)
    factor(2) = 2.250e+00_REAL64 - x + factor(2)
    factor(3) = 2.625e+00_REAL64 - x + factor(3)

    factor(:) = factor(:) * factor(:)

    retval = sum( factor(:) )

    return

  end function Beale_Function_REAL64

  pure function Beale_Function_REAL128 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL128), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL128) :: retval

    ! variables for this <function>
    real(kind=REAL128) :: factor(1:3)

    factor(1) = x * y
    factor(2) = y * factor(1)
    factor(3) = y * factor(2)

    factor(1) = 1.500e+00_REAL128 - x + factor(1)
    factor(2) = 2.250e+00_REAL128 - x + factor(2)
    factor(3) = 2.625e+00_REAL128 - x + factor(3)

    factor(:) = factor(:) * factor(:)

    retval = sum( factor(:) )

    return

  end function Beale_Function_REAL128


  ! Goldstein Price Function
  pure function Goldstein_Price_Function_REAL64 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL64), intent(in) :: x, y

    ! variables for this <function>
    real(kind=REAL64) :: factor(1:4)

    ! return value of this <function>
    real(kind=REAL64) :: retval

    factor(1) = x + y + 1.0e+0_REAL64
    factor(2) = 1.9e+1_REAL64 - 1.4e+1_REAL64*(x+y) + 3.0e+0_REAL64*(x*x+y*y) + 6.0e+0_REAL64*x*y
    factor(3) = 2.0e+0_REAL64*x - 3.0e+0_REAL64*y
    factor(4) = 1.8e+1_REAL64 - 3.2e+1_REAL64*x + 1.2e+1_REAL64*x*x + 4.8e+1_REAL64*y - 3.6e+1_REAL64*x*y + 2.7e+1_REAL64*y*y

    retval = &!
      (1.0e+0_REAL64 + factor(1) * factor(1) * factor(2)) * &!
      (3.0e+1_REAL64 + factor(3) * factor(3) * factor(4))

    return

  end function Goldstein_Price_Function_REAL64

  pure function Goldstein_Price_Function_REAL128 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL128), intent(in) :: x, y

    ! variables for this <function>
    real(kind=REAL128) :: factor(1:4)

    ! return value of this <function>
    real(kind=REAL128) :: retval

    factor(1) = x + y + 1.0e+0_REAL128
    factor(2) = 1.9e+1_REAL128 - 1.4e+1_REAL128*(x+y) + 3.0e+0_REAL128*(x*x+y*y) + 6.0e+0_REAL128*x*y
    factor(3) = 2.0e+0_REAL128*x - 3.0e+0_REAL128*y
    factor(4) = 1.8e+1_REAL128 - 3.2e+1_REAL128*x + 1.2e+1_REAL128*x*x + 4.8e+1_REAL128*y - 3.6e+1_REAL128*x*y + 2.7e+1_REAL128*y*y

    retval = &!
      (1.0e+0_REAL128 + factor(1) * factor(1) * factor(2)) * &!
      (3.0e+1_REAL128 + factor(3) * factor(3) * factor(4))

    return

  end function Goldstein_Price_Function_REAL128


  ! Rastrigin Function
  pure function Rastrigin_Function_REAL64 (n, x) result(retval)

    ! arguments for this <function>
    integer(kind=INT32 ), intent(in) :: n    ! number of the dimension
    real   (kind=REAL64), intent(in) :: x(1:n) ! independent variables

    ! return value of this <function>
    real(kind=REAL64) :: retval

    ! support variables for this <function>
    integer(kind=INT32) :: itr

    retval =  1.0e+01_REAL64 * real(n, kind=REAL64)

    do itr = 1, n, 1
      retval = retval + x(itr)*x(itr) - 1.0e+01_REAL64 * cos(CircCnst_REAL64_twice*x(itr))
    end do

    return

  end function Rastrigin_Function_REAL64

  pure function Rastrigin_Function_REAL128 (n, x) result(retval)

    ! arguments for this <function>
    integer(kind=INT32 ), intent(in) :: n    ! number of the dimension
    real   (kind=REAL128), intent(in) :: x(1:n) ! independent variables

    ! return value of this <function>
    real(kind=REAL128) :: retval

    ! support variables for this <function>
    integer(kind=INT32) :: itr

    retval =  1.0e+01_REAL128 * real(n, kind=REAL128)

    do itr = 1, n, 1
      retval = retval + x(itr)*x(itr) - 1.0e+01_REAL128 * cos(CircCnst_REAL128_twice*x(itr))
    end do

    return

  end function Rastrigin_Function_REAL128


  ! Rosenbrock Function
  pure function Rosenbrock_Function_REAL64 (n, x, a, b) result(retval)

    ! arguments for this <function>
    integer(kind=INT32 ), intent(in) :: n
    real   (kind=REAL64), intent(in) :: x(1:n), a, b

    ! variables for this <function>
    real(kind=REAL64) :: factor(1:2)

    ! support variables for this <function>
    integer(kind=INT32) :: itr

    ! return value of this <function>
    real(kind=REAL64) :: retval

    retval = 0.0e+00_REAL64

    do itr = 1, n-1, 1
      factor(1) = a - x(itr)
      factor(2) = x(itr + 1) - x(itr) * x(itr)
      retval    = retval +  factor(1) * factor(1) + b * factor(2) * factor(2)
    end do

    return

  end function Rosenbrock_Function_REAL64

  pure function Rosenbrock_Function_REAL128 (n, x, a, b) result(retval)

    ! arguments for this <function>
    integer(kind=INT32  ), intent(in) :: n
    real   (kind=REAL128), intent(in) :: x(1:n), a, b

    ! variables for this <function>
    real(kind=REAL128) :: factor(1:2)

    ! support variables for this <function>
    integer(kind=INT32) :: itr

    ! return value of this <function>
    real(kind=REAL128) :: retval

    retval = 0.0e+00_REAL128

    do itr = 1, n-1, 1
      factor(1) = a - x(itr)
      factor(2) = x(itr + 1) - x(itr) * x(itr)
      retval    = retval +  factor(1) * factor(1) + b * factor(2) * factor(2)
    end do

  end function Rosenbrock_Function_REAL128

end module mod_Test_Functions_for_Optimization

! -------------------------------------------------------------------------------------------------------------------------------- !
! End of Source Code                                                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !
