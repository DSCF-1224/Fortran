! 
! [CAUTION]
! This is NOT the original "nrtype.f90" written in the "Numerical Recipes in Fortran 90"
! 

! file created : 2018.10.29
! file updated : 2018.10.29

MODULE nrtype

	! Require all variables to be explicitly declared
	IMPLICIT NONE

	! integer numbers
	INTEGER, PARAMETER :: I1B = 1
	INTEGER, PARAMETER :: I2B = 2
	INTEGER, PARAMETER :: I4B = 4
	INTEGER, PARAMETER :: I8B = 8
	INTEGER, PARAMETER :: IDB = 8

		INTEGER( KIND=I8B ), PARAMETER :: I1B_length = HUGE( 1_I1B )*2+1
		INTEGER( KIND=I8B ), PARAMETER :: I2B_length = HUGE( 1_I2B )*2+1
		INTEGER( KIND=I8B ), PARAMETER :: I4B_length = HUGE( 1_I4B )*2+1

	! real numbers
	INTEGER, PARAMETER :: SPR = KIND(1.0)                                    ! single    precision real
	INTEGER, PARAMETER :: DPR = SELECTED_REAL_KIND( 2*PRECISION( 1.0_SPR ) ) ! double    precision real
	INTEGER, PARAMETER :: QPR = SELECTED_REAL_KIND( 4*PRECISION( 1.0_SPR ) ) ! quadruple precision real

	! complex numbers
	INTEGER, PARAMETER :: SPC = KIND(( 1.0e+0, 1.0e+0 )) ! single    precision real
	INTEGER, PARAMETER :: DPC = KIND(( 1.0d+0, 1.0d+0 )) ! double    precision real
	INTEGER, PARAMETER :: QPC = KIND(( 1.0q+0, 1.0q+0 )) ! quadruple precision real

	! default logical
	INTEGER, PARAMETER :: LGT = KIND(.TRUE.)

	! set public/private of subroutine
	PUBLIC :: TEST

	CONTAINS

	SUBROUTINE TEST

		INTEGER( kind=I1B ) :: i1b_test
		INTEGER( kind=I2B ) :: i2b_test
		INTEGER( kind=I4B ) :: i4b_test
		INTEGER( kind=I8B ) :: i8b_test
		REAL(    kind=SPR ) :: spr_test
		REAL(    kind=DPR ) :: dpr_test
		REAL(    kind=QPR ) :: qpr_test

		PRINT '(A)', '[DIGITS]'
			PRINT '(A,I4)', 'I1B :', DIGITS( i1b_test )
			PRINT '(A,I4)', 'I2B :', DIGITS( i2b_test )
			PRINT '(A,I4)', 'I4B :', DIGITS( i4b_test )
			PRINT '(A,I4)', 'I8B :', DIGITS( i8b_test )
			PRINT '(A,I4)', 'SPR :', DIGITS( spr_test )
			PRINT '(A,I4)', 'DPR :', DIGITS( dpr_test )
			PRINT '(A,I4)', 'QPR :', DIGITS( qpr_test )

		PRINT '(A)', '[EPSILON]'
			PRINT '(A,ES90.82e3)', 'SPR :', EPSILON(spr_test )
			PRINT '(A,ES90.82e3)', 'DPR :', EPSILON(dpr_test )
			PRINT '(A,ES90.82e3)', 'QPR :', EPSILON(qpr_test )

		PRINT '(A)', '[HUGE]'
			PRINT '(A,I20)',         'I1B :', HUGE( i1b_test )
			PRINT '(A,I20)',         'I2B :', HUGE( i2b_test )
			PRINT '(A,I20)',         'I4B :', HUGE( i4b_test )
			PRINT '(A,I20)',         'I8B :', HUGE( i8b_test )
			PRINT '(A,ES128.118e5)', 'SPR :', HUGE( spr_test )
			PRINT '(A,ES128.118e5)', 'DPR :', HUGE( dpr_test )
			PRINT '(A,ES128.118e5)', 'QPR :', HUGE( qpr_test )

		PRINT '(A)', '[length]'
			PRINT '(A,I20)', 'I1B :', I1b_length
			PRINT '(A,I20)', 'I2B :', I2b_length
			PRINT '(A,I20)', 'I4B :', I4b_length

		PRINT '(A)', '[MAXEXPONENT]'
			PRINT '(A,I6)', 'SPR  :', MAXEXPONENT( spr_test )
			PRINT '(A,I6)', 'DPR  :', MAXEXPONENT( dpr_test )
			PRINT '(A,I6)', 'QPR  :', MAXEXPONENT( qpr_test )

		PRINT '(A)', '[MINEXPONENT]'
			PRINT '(A,I6)', 'SPR  :', MINEXPONENT( spr_test )
			PRINT '(A,I6)', 'DPR  :', MINEXPONENT( dpr_test )
			PRINT '(A,I6)', 'QPR  :', MINEXPONENT( qpr_test )

		PRINT '(A)', '[PRECISION]'
			PRINT '(A,I6)', 'SPR  :', PRECISION( spr_test )
			PRINT '(A,I6)', 'DPR  :', PRECISION( dpr_test )
			PRINT '(A,I6)', 'QPR  :', PRECISION( qpr_test )

		PRINT '(A)', '[RADIX]'
			PRINT '(A,I2)', 'I1B :', RADIX( i1b_test )
			PRINT '(A,I2)', 'I2B :', RADIX( i2b_test )
			PRINT '(A,I2)', 'I4B :', RADIX( i4b_test )
			PRINT '(A,I2)', 'I8B :', RADIX( i8b_test )
			PRINT '(A,I2)', 'SPR :', RADIX( spr_test )
			PRINT '(A,I2)', 'DPR :', RADIX( dpr_test )
			PRINT '(A,I2)', 'QPR :', RADIX( qpr_test )

		PRINT '(A)', '[RANGE]'
			PRINT '(A,I)', 'I1B :', RANGE( i1b_test )
			PRINT '(A,I)', 'I2B :', RANGE( i2b_test )
			PRINT '(A,I)', 'I4B :', RANGE( i4b_test )
			PRINT '(A,I)', 'I8B :', RANGE( i8b_test )

		PRINT '(A)', '[TINY]'
			PRINT '(A,ES128.118e5)', 'SPR :', TINY( spr_test )
			PRINT '(A,ES128.118e5)', 'DPR :', TINY( dpr_test )
			PRINT '(A,ES128.118e5)', 'QPR :', TINY( qpr_test )

		RETURN

		! [DIGITS]
		! I1B :   7
		! I2B :  15
		! I4B :  31
		! I8B :  63
		! SPR :  24
		! DPR :  53
		! QPR : 113

		! [EPSILON]
		! SPR : 1.1920928955078125000000000000000000000000000000000000000000000000000000000000000000E-007
		! DPR : 2.2204460492503130808472633361816406250000000000000000000000000000000000000000000000E-016
		! QPR : 1.9259299443872358530559779425849273185381016482153881952399387955665588378906250000E-034
		
		! [HUGE]
		! I1B :                 127
		! I2B :               32767
		! I4B :          2147483647
		! I8B : 9223372036854775807
		! SPR : 3.4028234663852885981170418348451692543920658642411831922643415151383626785130669371252883023958979229933705687276279672E+00038
		! DPR : 1.7976931348623157081452742373170435679840613077466220928506319206037967368288905603988327685826575590844885547449231127E+00308
		! QPR : 1.1897314953572317650857593266280070161955504611916843650497190400971457489735349047319435468845227034567868997647685313E+04932
		
		! [MAXEXPONENT]
		! SPR  :   128
		! DPR  :  1024
		! QPR  : 16384
		
		! [MINEXPONENT]
		! SPR  :  -125
		! DPR  : -1021
		! QPR  :-16381
		
		! [PRECISION]
		! SPR  :     6
		! DPR  :    15
		! QPR  :    33
		
		! [RADIX]
		! I1B : 2
		! I2B : 2
		! I4B : 2
		! I8B : 2
		! SPR : 2
		! DPR : 2
		! QPR : 2
		
		! [RANGE]
		! I1B :           2
		! I2B :           4
		! I4B :           9
		! I8B :          18
		
		! [TINY]
		! SPR : 1.1754943508222875079687365372222456778186655567720875215087517062784172594547271728515625000000000000000000000000000000E-00038
		! DPR : 2.2250738585072013830902327173324040642195198525138913844848501335373054201270249680561777103071641192855167901143431664E-00308
		! QPR : 3.3621031431120935062626778173217526025950030875474599356257034317462611898078862692428557534807254114639363251626491547E-04932

	END SUBROUTINE
	
END MODULE