! [reference]
! Test functions for optimization - Wikipedia
! https://en.wikipedia.org/wiki/Test_functions_for_optimization
! https://www.sfu.ca/~ssurjano/optimization.html
! http://benchmarkfcns.xyz/fcns

module mod_Test_Functions_for_Optimization

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of <subroutine>s and <function>s in this <module>
  public :: Ackley_Func
  public :: Ackley_Func_N02
  public :: Ackley_Func_N03
  public :: Beale_Func
  public :: Bukin_Func_N06
  public :: CrossInTray_Func
  public :: Goldstein_Price_Func
  public :: Himmelblau_Func
  public :: HolderTable_Func
  public :: Levi_Func_N13
  public :: McCormick_Func
  public :: Rastrigin_Func
  public :: Rosenbrock_Func
  public :: Schaffer_Func_N02
  public :: Shubert_Func
  public :: ThreeHumpCamel_Func

  private :: Ackley_Func_REAL64,          Ackley_Func_REAL128
  private :: Ackley_Func_N02_REAL64,      Ackley_Func_N02_REAL128
  private :: Ackley_Func_N03_REAL64,      Ackley_Func_N03_REAL128
  private :: Beale_Func_REAL64,           Beale_Func_REAL128
  private :: Bukin_Func_N06_REAL64,       Bukin_Func_N06_REAL128
  private :: CrossInTray_Func_REAL64,     CrossInTray_Func_REAL128
  private :: Eason_Func_REAL64,           Eason_Func_REAL128
  private :: Goldstein_Price_Func_REAL64, Goldstein_Price_Func_REAL128
  private :: Himmelblau_Func_REAL64,      Himmelblau_Func_REAL128
  private :: Rastrigin_Func_REAL64,       Rastrigin_Func_REAL128
  private :: Rosenbrock_Func_REAL64,      Rosenbrock_Func_REAL128
  private :: Levi_Func_N13_REAL64,        Levi_Func_N13_REAL128
  private :: McCormick_Func_REAL64,       McCormick_Func_REAL128
  private :: Schaffer_Func_N02_REAL64,    Schaffer_Func_N02_REAL128
  private :: Shubert_Func_REAL64,         Shubert_Func_REAL128
  private :: ThreeHumpCamel_Func_REAL64,  ThreeHumpCamel_Func_REAL128

  ! constants for this <module>
  real(kind=REAL64),  parameter, private :: CircCnst_REAL64         = acos(-1.0e+00_REAL64 )
  real(kind=REAL128), parameter, private :: CircCnst_REAL128        = acos(-1.0e+00_REAL128)
  real(kind=REAL64),  parameter, private :: CircCnst_REAL64_twice   = 2.0e+00_REAL64  * CircCnst_REAL64
  real(kind=REAL128), parameter, private :: CircCnst_REAL128_twice  = 2.0e+00_REAL128 * CircCnst_REAL128
  real(kind=REAL64),  parameter, private :: CircCnst_REAL64_triple  = 3.0e+00_REAL64  * CircCnst_REAL64
  real(kind=REAL128), parameter, private :: CircCnst_REAL128_triple = 3.0e+00_REAL128 * CircCnst_REAL128
  real(kind=REAL64),  parameter, private :: CircCnst_REAL64_invs    = 1.0e+00_REAL64  / CircCnst_REAL64
  real(kind=REAL128), parameter, private :: CircCnst_REAL128_invs   = 1.0e+00_REAL128 / CircCnst_REAL128

  real(kind=REAL64),  parameter, private :: NapierCnst_REAL64  = exp(1.0e+00_REAL64 )
  real(kind=REAL128), parameter, private :: NapierCnst_REAL128 = exp(1.0e+00_REAL128)
  

  ! <interface>s for this <module>
  interface Ackley_Func
    module procedure Ackley_Func_REAL64
    module procedure Ackley_Func_REAL128
  end interface Ackley_Func

  interface Ackley_Func_N02
    module procedure Ackley_Func_N02_REAL64
    module procedure Ackley_Func_N02_REAL128
  end interface Ackley_Func_N02

  interface Ackley_Func_N03
    module procedure Ackley_Func_N03_REAL64
    module procedure Ackley_Func_N03_REAL128
  end interface Ackley_Func_N03

  interface Beale_Func
    module procedure Beale_Func_REAL64
    module procedure Beale_Func_REAL128
  end interface Beale_Func

  interface Bukin_Func_N06
    module procedure Bukin_Func_N06_REAL64
    module procedure Bukin_Func_N06_REAL128
  end interface Bukin_Func_N06

  interface CrossInTray_Func
    module procedure CrossInTray_Func_REAL64
    module procedure CrossInTray_Func_REAL128
  end interface CrossInTray_Func

  interface Eason_Func
    module procedure Eason_Func_REAL64
    module procedure Eason_Func_REAL128
  end interface Eason_Func

  interface EggHolder_Func
    module procedure EggHolder_Func_REAL64
    module procedure EggHolder_Func_REAL128
  end interface EggHolder_Func

  interface Goldstein_Price_Func
    module procedure Goldstein_Price_Func_REAL64
    module procedure Goldstein_Price_Func_REAL128
  end interface Goldstein_Price_Func

  interface Himmelblau_Func
    module procedure Himmelblau_Func_REAL64
    module procedure Himmelblau_Func_REAL128
  end interface Himmelblau_Func

  interface HolderTable_Func
    module procedure HolderTable_Func_REAL64
    module procedure HolderTable_Func_REAL128
  end interface HolderTable_Func

  interface Levi_Func_N13
    module procedure Levi_Func_N13_REAL64
    module procedure Levi_Func_N13_REAL128
  end interface Levi_Func_N13

  interface McCormick_Func
    module procedure McCormick_Func_REAL64
    module procedure McCormick_Func_REAL128
  end interface McCormick_Func

  interface Rastrigin_Func
    module procedure Rastrigin_Func_REAL64
    module procedure Rastrigin_Func_REAL128
  end interface Rastrigin_Func

  interface Rosenbrock_Func
    module procedure Rosenbrock_Func_REAL64
    module procedure Rosenbrock_Func_REAL128
  end interface Rosenbrock_Func

  interface Schaffer_Func_N02
      module procedure Schaffer_Func_N02_REAL64
      module procedure Schaffer_Func_N02_REAL128
  end interface Schaffer_Func_N02

  interface Shubert_Func
    module procedure Shubert_Func_REAL64
    module procedure Shubert_Func_REAL128
  end interface Shubert_Func

  interface ThreeHumpCamel_Func
    module procedure ThreeHumpCamel_Func_REAL64
    module procedure ThreeHumpCamel_Func_REAL128
  end interface ThreeHumpCamel_Func

  ! contained <subroutine>s and <function>s are below
  contains


  ! Ackley Function
  ! [range]            abs(x) <= 5 & abs(y) <= 5
  ! [optimal solution] x = y = 0
  ! [optimal value]    0

  pure function Ackley_Func_REAL64 (x, y) result(retval)

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

  end function Ackley_Func_REAL64

  pure function Ackley_Func_REAL128 (x, y) result(retval)

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

  end function Ackley_Func_REAL128


  ! Ackley Function N.02
  ! [range]            abs(x) <= 32 & abs(y) <= 32
  ! [optimal solution] x = y = 0
  ! [optimal value]    -200

  pure function Ackley_Func_N02_REAL64 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL64), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL64) :: retval

    retval = - 2.0e+002_REAL64 * exp( - 2.0e-001_REAL64 * sqrt(x*x + y*y) )
    return

  end function Ackley_Func_N02_REAL64

  pure function Ackley_Func_N02_REAL128 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL128), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL128) :: retval

    retval = - 2.0e+002_REAL128 * exp( - 2.0e-001_REAL128 * sqrt(x*x + y*y) )
    return

  end function Ackley_Func_N02_REAL128


  ! Ackley Function N.02
  ! [range]            abs(x) <= 32 & abs(y) <= 32
  ! [optimal solution] (x,y)=(+0.682584587365898, −0.36075325513719)
  ! [optimal solution] (x,y)=(-0.682584587365898, −0.36075325513719)
  ! [optimal value]    -195.629028238419

  pure function Ackley_Func_N03_REAL64 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL64), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL64) :: retval

    retval                         &!
    = Ackley_Func_N02_REAL64(x, y) &!
    + 5.0e+000_REAL64 * exp( cos(3.0e+000_REAL64 * x) + sin(3.0e+000_REAL64 * y) )
    return

  end function Ackley_Func_N03_REAL64

  pure function Ackley_Func_N03_REAL128 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL128), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL128) :: retval

    retval                         &!
    = Ackley_Func_N02_REAL128(x, y) &!
    + 5.0e+000_REAL128 * exp( cos(3.0e+000_REAL128 * x) + sin(3.0e+000_REAL128 * y) )
    return

  end function Ackley_Func_N03_REAL128


  ! Beale Function
  ! [range]            abs(x) <= 4.5 & abs(y) <= 4.5
  ! [optimal solution] x = 3.0, y = 0.5
  ! [optimal value]    0

  pure function Beale_Func_REAL64 (x, y) result(retval)

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

  end function Beale_Func_REAL64

  pure function Beale_Func_REAL128 (x, y) result(retval)

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

  end function Beale_Func_REAL128


  ! Bukin Function N.6
  ! [range]            -15 <= x <= -5, -3 <= x <= 3
  ! [optimal solution] x = -10.0, y = +1.0
  ! [optimal value]    0

  pure function Bukin_Func_N06_REAL64 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL64), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL64) :: retval

    retval = 1.0e+2_REAL64 * sqrt( abs(y - 1.0e-2_REAL64*x*x) ) + 1.0e-2_REAL64 * abs(x + 1.0e+1_REAL64)

    return

  end function Bukin_Func_N06_REAL64

  pure function Bukin_Func_N06_REAL128 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL128), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL128) :: retval

    retval = 1.0e+2_REAL128 * sqrt( abs(y - 1.0e-2_REAL128*x*x) ) + 1.0e-2_REAL128 * abs(x + 1.0e+1_REAL128)

    return

  end function Bukin_Func_N06_REAL128


  ! Cross in tray Function
  ! [range]            abs(x) <= 10, abs(y) <= 10
  ! [optimal solution] (x,y)=(+1.34941,+1.34941)
  ! [optimal solution] (x,y)=(-1.34941,+1.34941)
  ! [optimal solution] (x,y)=(-1.34941,-1.34941)
  ! [optimal solution] (x,y)=(+1.34941,-1.34941)
  ! [optimal value]    -2.06261

  pure function CrossInTray_Func_REAL64 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL64), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL64) :: retval

    retval =  abs( 1.0e+02_REAL64 - sqrt(x*x + y*y) * CircCnst_REAL64_invs )
    retval = -1.0e-04_REAL64 * ( abs( sin(x) * sin(y) * exp( retval ) ) + 1.0e+00_REAL64 ) ** 1.0e-01_REAL64

    return

  end function CrossInTray_Func_REAL64

  pure function CrossInTray_Func_REAL128 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL128), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL128) :: retval

    retval =  abs( 1.0e+02_REAL128 - sqrt(x*x + y*y) * CircCnst_REAL128_invs )
    retval = -1.0e-04_REAL128 * ( abs( sin(x) * sin(y) * exp( retval ) ) + 1.0e+00_REAL128 ) ** 1.0e-01_REAL128

    return

  end function CrossInTray_Func_REAL128


  ! Eason Function
  ! [range]            abs(x) <= 100 & abs(y) <= 100
  ! [optimal solution] (x,y)=(\pi,\pi)
  ! [optimal value]    -1

  pure function Eason_Func_REAL64 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL64), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL64) :: retval

    ! variables for this <function>
    real(kind=REAL64) :: factor(1:2)

    factor(1) = x - CircCnst_REAL64
    factor(2) = y - CircCnst_REAL64

    retval = - cos(x) * cos(y) * exp( - factor(1) * factor(1) - factor(2) * factor(2) )

    return

  end function Eason_Func_REAL64

  pure function Eason_Func_REAL128 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL128), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL128) :: retval

    ! variables for this <function>
    real(kind=REAL128) :: factor(1:2)

    factor(1) = x - CircCnst_REAL128
    factor(2) = y - CircCnst_REAL128

    retval = - cos(x) * cos(y) * exp( - factor(1) * factor(1) - factor(2) * factor(2) )

    return

  end function Eason_Func_REAL128


  ! EggHolder Function
  ! [range]            abs(x) <= 512 & abs(y) <= 512
  ! [optimal solution] (x,y)=(512,404.2319)
  ! [optimal value]    -959.6407

  pure function EggHolder_Func_REAL64 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL64), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL64) :: retval

    ! variables for this <function>
    real(kind=REAL64) :: sum

    sum = y + 4.7e+01_REAL64

    retval = &!
      - sum * sin( sqrt( abs( 5.0e-01_REAL64 * x + sum ) ) ) &!
      - x   * sin( sqrt( abs(                  x - sum ) ) )

    return

  end function EggHolder_Func_REAL64

  pure function EggHolder_Func_REAL128 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL128), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL128) :: retval

    ! variables for this <function>
    real(kind=REAL128) :: sum

    sum = y + 4.7e+01_REAL128

    retval = &!
      - sum * sin( sqrt( abs( 5.0e-01_REAL128 * x + sum ) ) ) &!
      - x   * sin( sqrt( abs(                   x - sum ) ) )

    return

  end function EggHolder_Func_REAL128

  ! Goldstein Price Function
  ! [range]            abs(x) <= 2 & abs(y) <= 2
  ! [optimal solution] x = 0.0, y = -1.0
  ! [optimal value]    3

  pure function Goldstein_Price_Func_REAL64 (x, y) result(retval)

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

  end function Goldstein_Price_Func_REAL64

  pure function Goldstein_Price_Func_REAL128 (x, y) result(retval)

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

  end function Goldstein_Price_Func_REAL128


  ! Himmelblau Function
  ! [range]            abs(x) <= 5, abs(y) <= 5
  ! [optimal solution] (x,y)=(+3.000000,+2.000000)
  ! [optimal solution] (x,y)=(-2.805118,+3.131312)
  ! [optimal solution] (x,y)=(-3.779310,-3.283186)
  ! [optimal solution] (x,y)=(+3.584428,-1.848126)
  ! [optimal value]    0

  pure function Himmelblau_Func_REAL64 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL64), intent(in) :: x, y

    ! variables for this <function>
    real(kind=REAL64) :: factor(1:2)

    ! return value of this <function>
    real(kind=REAL64) :: retval

    factor(1) = x * x + y - 1.1e+1_REAL64
    factor(2) = x + y * y - 7.0e+0_REAL64

    retval = factor(1) * factor(1) + factor(2) * factor(2)

    return

  end function Himmelblau_Func_REAL64

  pure function Himmelblau_Func_REAL128 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL128), intent(in) :: x, y

    ! variables for this <function>
    real(kind=REAL128) :: factor(1:2)

    ! return value of this <function>
    real(kind=REAL128) :: retval

    factor(1) = x * x + y - 1.1e+1_REAL128
    factor(2) = x + y * y - 7.0e+0_REAL128

    retval = factor(1) * factor(1) + factor(2) * factor(2)

    return

  end function Himmelblau_Func_REAL128


  ! Holder Table Function
  ! [range]            abs(x) <= 5, abs(y) <= 5
  ! [optimal solution] (x,y)=(+8.055502,+9.66459)
  ! [optimal solution] (x,y)=(-8.055502,+9.66459)
  ! [optimal solution] (x,y)=(-8.055502,-9.66459)
  ! [optimal solution] (x,y)=(+8.055502,-9.66459)
  ! [optimal value]    -19.2085

  pure function HolderTable_Func_REAL64 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL64), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL64) :: retval

    retval = - abs( sin(x) * cos(y) * exp( abs( 1.0e+00_REAL64 - sqrt(x*x + y*y) * CircCnst_REAL64_invs ) ) )

    return

  end function HolderTable_Func_REAL64

  pure function HolderTable_Func_REAL128 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL128), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL128) :: retval

    retval = - abs( sin(x) * cos(y) * exp( abs( 1.0e+00_REAL128 - sqrt(x*x + y*y) * CircCnst_REAL64_invs ) ) )

    return

  end function HolderTable_Func_REAL128


  ! Levi Function version 13
  ! [range]            abs(x) <= 10, abs(y) <= 10
  ! [optimal solution] x = y = 1.0
  ! [optimal value]    0

  pure function Levi_Func_N13_REAL64 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL64), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL64) :: retval

    ! support variables for this <function>
    real(kind=REAL64) :: factor(1:5)

    factor(1) = sin(CircCnst_REAL64_triple * x)
    factor(2) = x - 1.0e+00_REAL64
    factor(3) = sin(CircCnst_REAL64_triple * y)
    factor(4) = y - 1.0e+00_REAL64
    factor(5) = sin(CircCnst_REAL64_twice * y)

    retval &!
    = factor(1) * factor(1) &!
    + factor(2) * factor(2) * ( 1.0e+00_REAL64 + factor(3) * factor(3) ) &!
    + factor(4) * factor(4) * ( 1.0e+00_REAL64 + factor(5) * factor(5) )

    return

  end function Levi_Func_N13_REAL64

  pure function Levi_Func_N13_REAL128 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL128), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL128) :: retval

    ! support variables for this <function>
    real(kind=REAL128) :: factor(1:5)

    factor(1) = sin(CircCnst_REAL128_triple* x)
    factor(2) = x - 1.0e+00_REAL128
    factor(3) = sin(CircCnst_REAL128_triple* y)
    factor(4) = y - 1.0e+00_REAL128
    factor(5) = sin(CircCnst_REAL128_twice * y)

    retval &!
    = factor(1) * factor(1) &!
    + factor(2) * factor(2) * ( 1.0e+00_REAL64 + factor(3) * factor(3) ) &!
    + factor(4) * factor(4) * ( 1.0e+00_REAL64 + factor(5) * factor(5) )

    return

  end function Levi_Func_N13_REAL128


  ! McCormick Function
  ! [range]            abs(x) <= 10, abs(y) <= 10
  ! [optimal solution] (x,y)=(-0.54719,-1.54719)
  ! [optimal value]    -1.9133

  pure function McCormick_Func_REAL64 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL64), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL64) :: retval

    retval = sin(x+y) + (x-y)*(x-y) - 1.5e+00_REAL64 * x + 2.5e+00_REAL64 * y + 1.0e+00_REAL64

    return

  end function McCormick_Func_REAL64

  pure function McCormick_Func_REAL128 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL128), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL128) :: retval

    retval = sin(x+y) + (x-y)*(x-y) - 1.5e+00_REAL128 * x + 2.5e+00_REAL128 * y + 1.0e+00_REAL128

    return

  end function McCormick_Func_REAL128


  ! Rastrigin Function
  ! [range]            abs(x_i) <= 5.12, (i=1,2,...,n)
  ! [optimal solution] x_i = 0,          (i=1,2,...,n)
  ! [optimal value]    0

  pure function Rastrigin_Func_REAL64 (n, x) result(retval)

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

  end function Rastrigin_Func_REAL64

  pure function Rastrigin_Func_REAL128 (n, x) result(retval)

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

  end function Rastrigin_Func_REAL128


  ! Rosenbrock Function
  ! [range]            abs(x_i) < \infinity, (i=1,2,...,n)
  ! [optimal solution] x_i = 1,              (i=1,2,...,n)
  ! [optimal value]    0
  ! [sample]           Rosenbrock_Func(2, (/cdx, cdy/), 1.0e+000_REAL64, 1.0e+002_REAL64)

  pure function Rosenbrock_Func_REAL64 (n, x, a, b) result(retval)

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

  end function Rosenbrock_Func_REAL64

  pure function Rosenbrock_Func_REAL128 (n, x, a, b) result(retval)

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

  end function Rosenbrock_Func_REAL128


  ! Schaffer Function N.2
  ! [range]            abs(x) <= 100, abs(y) <= 100
  ! [optimal solution] (x,y)=(0,0)
  ! [optimal value]    0

  pure function Schaffer_Func_N02_REAL64 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL64), intent(in) :: x, y

    ! variables for this <function>
    real(kind=REAL64) :: power(1:4)

    ! return value of this <function>
    real(kind=REAL64) :: retval

    retval = 5.0e-01_REAL64

    power(1) = x * x
    power(2) = y * y
    power(3) = sin( power(1) - power(2) )
    power(4) = 1.0e+00_REAL64 + 1.0e-03_REAL64 * ( power(1) + power(2) )

    retval = retval + ( power(3) * power(3) - retval ) / ( power(4) * power(4) )

    return

  end function Schaffer_Func_N02_REAL64

  pure function Schaffer_Func_N02_REAL128 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL128), intent(in) :: x, y

    ! variables for this <function>
    real(kind=REAL128) :: power(1:4)

    ! return value of this <function>
    real(kind=REAL128) :: retval

    retval = 5.0e-01_REAL128

    power(1) = x * x
    power(2) = y * y
    power(3) = sin( power(1) - power(2) )
    power(4) = 1.0e+00_REAL128 + 1.0e-03_REAL128 * ( power(1) + power(2) )

    retval = retval + ( power(3) * power(3) - retval ) / ( power(4) * power(4) )

    return

  end function Schaffer_Func_N02_REAL128


  ! Shubert Function
  ! [range]            abs(x) <= 10, abs(y) <= 10
  ! [optimal solution] ???
  ! [optimal value]    -186.7309

  pure function Shubert_Func_REAL64 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL64), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL64) :: retval

    ! support variables for this <function>
    integer(INT32) :: itr

    retval = 0.0e+000_REAL64

    do itr = 1_INT32, 5_INT32, 1_INT32
      retval &!
      = retval                                 &!
      + itr * cos( (itr + 1_INT32) * x + itr ) &!
      * itr * cos( (itr + 1_INT32) * y + itr )
    end do

    return

  end function Shubert_Func_REAL64

  pure function Shubert_Func_REAL128 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL128), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL128) :: retval

    ! support variables for this <function>
    integer(INT32) :: itr

    retval = 0.0e+000_REAL128

    do itr = 1_INT32, 5_INT32, 1_INT32
      retval &!
      = retval                                 &!
      + itr * cos( (itr + 1_INT32) * x + itr ) &!
      * itr * cos( (itr + 1_INT32) * y + itr )
    end do

    return

  end function Shubert_Func_REAL128


  ! Three-hump camel Function
  ! [range]            abs(x) <= 5, abs(y) <= 5
  ! [optimal solution] (x,y)=(0,0)
  ! [optimal value]    0

  pure function ThreeHumpCamel_Func_REAL64 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL64), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL64) :: retval

    retval &!
      = 2.00e+0_REAL64 * x * x                &!
      - 1.05e+0_REAL64 * x * x * x * x        &!
      + x * x * x * x * x * x / 6.0e+0_REAL64 &!
      + x * y                                 &!
      + y * y 

    return

  end function ThreeHumpCamel_Func_REAL64

  pure function ThreeHumpCamel_Func_REAL128 (x, y) result(retval)

    ! arguments for this <function>
    real(kind=REAL128), intent(in) :: x, y

    ! return value of this <function>
    real(kind=REAL128) :: retval

    retval &!
      = 2.00e+0_REAL128 * x * x                &!
      - 1.05e+0_REAL128 * x * x * x * x        &!
      + x * x * x * x * x * x / 6.0e+0_REAL128 &!
      + x * y                                  &!
      + y * y 

    return

  end function ThreeHumpCamel_Func_REAL128

end module mod_Test_Functions_for_Optimization

! -------------------------------------------------------------------------------------------------------------------------------- !
! End of Source Code                                                                                                               !
! -------------------------------------------------------------------------------------------------------------------------------- !
