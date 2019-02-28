program main

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! require all variables to be explicitly declared
  implicit none

  ! constants for this <program>
  character( len= 3, kind=1 ), parameter :: fmt_NameFunc         = '(A)'
  character( len=17, kind=1 ), parameter :: fmt_CheckDigits      = '(A7,1X,":",1X,I3)'
  character( len=24, kind=1 ), parameter :: fmt_CheckEpsilon     = '(A7,1X,":",1X,ES24.16e4)'
  character( len=18, kind=1 ), parameter :: fmt_CheckHuge_INT    = '(A7,1X,":",1X,I24)'
  character( len=24, kind=1 ), parameter :: fmt_CheckHuge_REAL   = '(A7,1X,":",1X,ES24.16e4)'
  character( len=17, kind=1 ), parameter :: fmt_CheckMaxExponent = '(A7,1X,":",1X,I6)'
  character( len=17, kind=1 ), parameter :: fmt_CheckMinExponent = fmt_CheckMaxExponent

  ! variables for this <program>/
  integer( kind=INT16 ) :: val_INT16
  integer( kind=INT32 ) :: val_INT32
  integer( kind=INT64 ) :: val_INT64
  real( kind=REAL32 )   :: val_REAL32
  real( kind=REAL64 )   :: val_REAL64
  real( kind=REAL128 )  :: val_REAL128

  !--- main process is below ---!

  ! STEP.01
  ! check the return value of [DIGITS]
  write( unit=OUTPUT_UNIT, fmt=fmt_NameFunc,    advance='yes' ) '[DIGITS]'
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckDigits, advance='yes' ) 'INT16',   DIGITS( val_INT16   )
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckDigits, advance='yes' ) 'INT32',   DIGITS( val_INT32   )
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckDigits, advance='yes' ) 'INT64',   DIGITS( val_INT64   )
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckDigits, advance='yes' ) 'REAL32',  DIGITS( val_REAL32  )
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckDigits, advance='yes' ) 'REAL64',  DIGITS( val_REAL64  )
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckDigits, advance='yes' ) 'REAL128', DIGITS( val_REAL128 )

  ! STEP.02
  ! check the return value of [EPSILON]
  write( unit=OUTPUT_UNIT, fmt=fmt_NameFunc,     advance='yes' ) '[EPSILON]'
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckEpsilon, advance='yes' ) 'REAL32',  EPSILON( val_REAL32  )
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckEpsilon, advance='yes' ) 'REAL64',  EPSILON( val_REAL64  )
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckEpsilon, advance='yes' ) 'REAL128', EPSILON( val_REAL128 )

  ! STEP.03
  ! check the return value of [HUGE]
  write( unit=OUTPUT_UNIT, fmt=fmt_NameFunc,       advance='yes' ) '[HUGE]'
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckHuge_INT,  advance='yes' ) 'INT16',   HUGE( val_INT16   )
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckHuge_INT,  advance='yes' ) 'INT32',   HUGE( val_INT32   )
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckHuge_INT,  advance='yes' ) 'INT64',   HUGE( val_INT64   )
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckHuge_REAL, advance='yes' ) 'REAL32',  HUGE( val_REAL32  )
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckHuge_REAL, advance='yes' ) 'REAL64',  HUGE( val_REAL64  )
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckHuge_REAL, advance='yes' ) 'REAL128', HUGE( val_REAL128 )

  ! STEP.04
  ! check the return value of [MAXEXPONENT]
  write( unit=OUTPUT_UNIT, fmt=fmt_NameFunc,         advance='yes' ) '[MAXEXPONENT]'
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckMaxExponent, advance='yes' ) 'REAL32',  MAXEXPONENT( val_REAL32  )
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckMaxExponent, advance='yes' ) 'REAL64',  MAXEXPONENT( val_REAL64  )
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckMaxExponent, advance='yes' ) 'REAL128', MAXEXPONENT( val_REAL128 )

  ! STEP.05
  ! check the return value of [MINEXPONENT]
  write( unit=OUTPUT_UNIT, fmt=fmt_NameFunc,         advance='yes' ) '[MINEXPONENT]'
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckMinExponent, advance='yes' ) 'REAL32',  MINEXPONENT( val_REAL32  )
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckMinExponent, advance='yes' ) 'REAL64',  MINEXPONENT( val_REAL64  )
  write( unit=OUTPUT_UNIT, fmt=fmt_CheckMinExponent, advance='yes' ) 'REAL128', MINEXPONENT( val_REAL128 )

  ! STEP.02
  ! temporary stop
  read *

end program main