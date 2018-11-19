! file created : 2018.10.29
! file updated : 2018.11.19


MODULE PrandtlMeyerCnst

	! Require all variables to be explicitly declared
	IMPLICIT NONE

	! local constants in this <MODULE>
	DOUBLE PRECISION, PARAMETER :: CircCnst     = ACOS( -1.0D+0 )
	DOUBLE PRECISION, PARAMETER :: Coef_RAD2DEG = 1.8D+2 / ACOS( -1.0D+0 )
	DOUBLE PRECISION, PARAMETER :: Coef_DEG2RAD = 1.0D+0 / Coef_RAD2DEG

	CONTAINS

	FUNCTION PMfuncMach( mach ) RESULT( retval )

		! argument for this <FUNCTION>
		DOUBLE PRECISION, INTENT(IN) :: mach ! mach number

		! return value of this <FUNCTION>
		DOUBLE PRECISION :: retval

		IF( mach .LT. 1.0D+0 ) THEN
			STOP "the Mach number for Prandtl-Meyer function must be greater than 1.0 !"
		ELSE
			retval= SQRT( mach*mach-1.0D+0 )
			RETURN
		ENDIF
		
	END FUNCTION PMfuncMach



	FUNCTION PMfuncSPHR( sphr ) RESULT( retval )

		! argument for this <FUNCTION>
		DOUBLE PRECISION, INTENT(IN) :: sphr ! specific heat ratio

		! return value of this <FUNCTION>
		DOUBLE PRECISION :: retval

		IF( sphr .LT. 1.0D+0 ) THEN
			STOP "the specific heat ratio must be greater than 1.0 !"
		ELSE
			retval= SQRT( ( sphr+1.0D+0 ) / ( sphr-1.0D+0 ) )
			RETURN
		ENDIF

	END FUNCTION PMfuncSPHR



	FUNCTION PMfuncMax( sphr, mode ) RESULT( retval )

		! argument for this <FUNCTION>
		CHARACTER( LEN= 3 ), INTENT(IN) :: mode ! "rad" or "deg"
		DOUBLE PRECISION,    INTENT(IN) :: sphr ! specific heat ratio [-]

		! return value of this <FUNCTION>
		DOUBLE PRECISION :: retval

		! STEP.01
		! calculate the maximum value of Prandlt-Meyer function [rad]
		retval= 5.0D-1 * CircCnst * ( PMfuncSPHR( sphr ) - 1.0D+0 )

		! STEP.02
		! set the return value
		IF ( mode .EQ. "rad" ) THEN
			RETURN
		ELSEIF ( mode .EQ. "deg" ) THEN
			retval = Coef_RAD2DEG * retval ! convert radian to degree
			RETURN
		ENDIF

	END FUNCTION PMfuncMax


END MODULE PrandtlMeyerCnst



MODULE PrandtlMeyerNrml

	! imported <MODULE>
	USE PrandtlMeyerCnst

	! Require all variables to be explicitly declared
	IMPLICIT NONE

	CONTAINS

	! Prandlt-Meyer function [deg]
	FUNCTION PMfuncDeg( mach, sphr ) RESULT( pmf )

		! argument for this <FUNCTION>
		DOUBLE PRECISION, INTENT(IN) :: mach ! mach number
		DOUBLE PRECISION, INTENT(IN) :: sphr ! specific heat ratio

		! return value of this <FUNCTION>
		DOUBLE PRECISION :: pmf
		pmf= PMfuncRad( MACH= mach, SPHR= sphr ) * Coef_RAD2DEG

	END FUNCTION PMfuncDeg


	! Prandlt-Meyer function [rad]
	FUNCTION PMfuncRad( mach, sphr ) RESULT( pmf )

		! argument for this <FUNCTION>
		DOUBLE PRECISION, INTENT(IN) :: mach ! mach number
		DOUBLE PRECISION, INTENT(IN) :: sphr ! specific heat ratio

		! return value of this <FUNCTION>
		DOUBLE PRECISION :: pmf

		! local variables for this <FUNCTION>
		DOUBLE PRECISION :: buf_mach
		DOUBLE PRECISION :: buf_sphr

		! STEP.01
		buf_mach = PMfuncMach( MACH= mach )
		buf_sphr = PMfuncSPHR( SPHR= sphr )

		! STEP.02
		pmf= buf_sphr * ATAN( buf_mach / buf_sphr ) - ATAN( buf_mach )

	END FUNCTION PMfuncRad

END MODULE PrandtlMeyerNrml



! inverse function of Prandlt-Meyer function
MODULE PrandtlMeyerInvs

	! imported <MODULE>
	USE PrandtlMeyerCnst
	USE PrandtlMeyerNrml

	! Require all variables to be explicitly declared
	IMPLICIT NONE

	! local constants in this <MODULE>
	INTEGER( KIND= 4 ), PARAMETER :: Lim_Iteration= 1000

	CONTAINS

	! inverse function of Prandlt-Meyer function
	FUNCTION iPMfuncBisection( rad, deg, sphr, err ) RESULT( mach )

		! arguments for this <FUNCTION>
		DOUBLE PRECISION, INTENT(IN), OPTIONAL :: rad  ! Prandlt-Meyer function [rad]
		DOUBLE PRECISION, INTENT(IN), OPTIONAL :: deg  ! Prandlt-Meyer function [deg]
		DOUBLE PRECISION, INTENT(IN)           :: sphr ! specific heat ratio    [-]
		DOUBLE PRECISION, INTENT(IN), OPTIONAL :: err  ! allowable error        [-]

		! return value of this <FUNCTION>
		DOUBLE PRECISION :: mach

		! local variables for this <FUNCTION>
		DOUBLE PRECISION :: err_allowable ! allowable error
		DOUBLE PRECISION :: mach_min      ! minimum  of the range of solution
		DOUBLE PRECISION :: mach_mid      ! midpoint of the range of solution
		DOUBLE PRECISION :: mach_max      ! maximum  of the range of solution
		DOUBLE PRECISION :: pmf_rad_min   ! minimum  of the range of Prandlt-Meyer function [rad]
		DOUBLE PRECISION :: pmf_rad_mid   ! midpoint of the range of Prandlt-Meyer function [rad]
		DOUBLE PRECISION :: pmf_rad_max   ! maximum  of the range of Prandlt-Meyer function [rad]
		DOUBLE PRECISION :: pmf_rad_trg   ! Prandlt-Meyer function [rad] of the target

		LOGICAL( KIND= 1 ) :: isRad    = .FALSE.
		LOGICAL( KIND= 1 ) :: isDeg    = .FALSE.
		LOGICAL( KIND= 1 ) :: isTooBig = .FALSE.

		! STEP.01
		! check the arguments
		
			! STEP.01.01
			! whether radian or degree
			IF( PRESENT( deg ) ) isDeg = .TRUE.
			IF( PRESENT( rad ) ) isRad = .TRUE.

			IF( isDeg .AND. isRad ) STOP "select whether `degree` or `radian`!"

			IF( isDeg ) THEN
				pmf_rad_trg = Coef_DEG2RAD * deg
			ELSE
				pmf_rad_trg = rad
			ENDIF

			! STEP.01.02
			! check the `specific heat ratio`
			IF( isDeg .AND. ( deg .GE. PMfuncMax( SPHR= sphr, MODE="deg" ) ) ) isTooBig = .TRUE.
			IF( isRad .AND. ( rad .GE. PMfuncMax( SPHR= sphr, MODE="rad" ) ) ) isTooBig = .TRUE.

			IF( isTooBig ) STOP "angle for inverse Prandlt-Meyer function is too big!"

			! STEP.01.03
			! how about the allowable error
			IF ( PRESENT( err ) ) THEN
				err_allowable= err
			ELSE
				err_allowable= 1.0D+1 * EPSILON( err_allowable )
			ENDIF

		! STEP.02
		! calculate inverse function of Prandlt-Meyer function

			! STEP.02.01
			! initialize the range of Mach number and so on
			mach_min = 1.0D+0
			mach_max = 1.0D+4

			! STEP.02.02
			! calculate the target using successive bisection method
			IF( pmf_rad_trg .EQ. 0.0D+0 ) THEN
				mach= mach_min
				RETURN
			ENDIF

			DO WHILE( mach_max - mach_min .GT. err_allowable * mach_min )
				
				! calculate the midpoint
				mach_mid = 5.0D-1 * ( mach_min + mach_max )

				! calculate the Prandtl-Meyer function at the each points
				pmf_rad_min = PMfuncRad( MACH= mach_min, SPHR= sphr ) - pmf_rad_trg
				pmf_rad_mid = PMfuncRad( MACH= mach_mid, SPHR= sphr ) - pmf_rad_trg
				pmf_rad_max = PMfuncRad( MACH= mach_max, SPHR= sphr ) - pmf_rad_trg

				! update the range for successive bisection method
				IF( pmf_rad_min * pmf_rad_mid .GT. 0.0D+0 ) THEN
					mach_min = mach_mid
				ELSE
					mach_max = mach_mid
				ENDIF

			ENDDO

			mach= mach_mid
			RETURN

	END FUNCTION iPMfuncBisection

END MODULE PrandtlMeyerInvs

PROGRAM MAIN

	! imported <MODULE>s
	USE PrandtlMeyerNrml
	USE PrandtlMeyerInvs

	! Require all variables to be explicitly declared
	IMPLICIT NONE


	! constants in this <PROGRAM>
	DOUBLE PRECISION, PARAMETER :: mach_step = 1.0D-1
	DOUBLE PRECISION, PARAMETER :: sphr      = 1.4D+0

	INTEGER( KIND= 4 ), PARAMETER :: UnitNum= 10

	! support local variables in this <PROGRAM>
	DOUBLE PRECISION :: mach      ! mach number
	DOUBLE PRECISION :: mach_invs ! mach number (inverse)
	DOUBLE PRECISION :: pmf_deg   ! Prandtl-Meyer function [deg]
	DOUBLE PRECISION :: pmf_rad   ! Prandtl-Meyer function [rad]
	DOUBLE PRECISION :: error     ! relative error between mach number and inverse of Prandlt-Meyer function

	INTEGER( KIND= 4 ) :: itr_mach
	INTEGER( KIND= 4 ) :: val_stat

	! STEP.01
	OPEN( UNIT= UnitNum, DEFAULTFILE= path_fldr_save, FILE= path_file_save, IOSTAT= val_stat, ACTION= 'WRITE', STATUS= 'REPLACE' )

	! STEP.02

		! STEP.02.01
		WRITE( UNIT= UnitNum, FMT= '(A)', ADVANCE= 'YES', IOSTAT= val_stat ) 'itr,MACH,PMF[rad],PMF[deg],inverse,relative error'

		! STEP.02.02
		DO itr_mach= 0, 900, 1
			mach      = 1.0D+0 + mach_step * REAL( itr_mach, KIND= KIND( mach_step ) )
			pmf_deg   = PMfuncDeg( MACH= mach, SPHR= sphr )
			pmf_rad   = PMfuncRad( MACH= mach, SPHR= sphr )
			mach_invs = iPMfuncBisection( DEG= pmf_deg, SPHR= sphr )
			error     = ABS( ( mach - mach_invs ) / mach )
			WRITE( UNIT= UnitNum, FMT='(I3,",",ES23.15,4(",",ES23.15))' ) itr_mach, mach, pmf_rad, pmf_deg, mach_invs, error
		ENDDO

	! STEP.03
	CLOSE( UNIT= UnitNum, STATUS= 'KEEP', IOSTAT= val_stat )


END PROGRAM MAIN