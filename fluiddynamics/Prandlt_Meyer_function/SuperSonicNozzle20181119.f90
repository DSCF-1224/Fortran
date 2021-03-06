! code created : 2018.11.19
! code updated : 2018.11.19


! supports for function of Prandtl-Meyer function
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
		! calculate the maximum value of Prandtl-Meyer function [rad]
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



! function of Prandtl-Meyer function
MODULE PrandtlMeyerNrml

	! imported <MODULE>
	USE PrandtlMeyerCnst

	! Require all variables to be explicitly declared
	IMPLICIT NONE

	CONTAINS

	! Prandtl-Meyer function [deg]
	FUNCTION PMfuncDeg( mach, sphr ) RESULT( pmf )

		! argument for this <FUNCTION>
		DOUBLE PRECISION, INTENT(IN) :: mach ! mach number
		DOUBLE PRECISION, INTENT(IN) :: sphr ! specific heat ratio

		! return value of this <FUNCTION>
		DOUBLE PRECISION :: pmf

		pmf= PMfuncRad( MACH= mach, SPHR= sphr ) * Coef_RAD2DEG

	END FUNCTION PMfuncDeg


	! Prandtl-Meyer function [rad]
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



! inverse function of Prandtl-Meyer function
MODULE PrandtlMeyerInvs

	! imported <MODULE>
	USE PrandtlMeyerCnst
	USE PrandtlMeyerNrml

	! Require all variables to be explicitly declared
	IMPLICIT NONE

	! local constants in this <MODULE>
	INTEGER( KIND= 4 ), PARAMETER :: Lim_Iteration= 1000

	CONTAINS

	! inverse function of Prandtl-Meyer function
	FUNCTION iPMfuncBisection( rad, deg, sphr, err ) RESULT( mach )

		! arguments for this <FUNCTION>
		DOUBLE PRECISION, INTENT(IN), OPTIONAL :: rad  ! Prandtl-Meyer function [rad]
		DOUBLE PRECISION, INTENT(IN), OPTIONAL :: deg  ! Prandtl-Meyer function [deg]
		DOUBLE PRECISION, INTENT(IN)           :: sphr ! specific heat ratio    [-]
		DOUBLE PRECISION, INTENT(IN), OPTIONAL :: err  ! allowable error        [-]

		! return value of this <FUNCTION>
		DOUBLE PRECISION :: mach

		! local variables for this <FUNCTION>
		DOUBLE PRECISION :: err_allowable ! allowable error
		DOUBLE PRECISION :: mach_min      ! minimum  of the range of solution
		DOUBLE PRECISION :: mach_mid      ! midpoint of the range of solution
		DOUBLE PRECISION :: mach_max      ! maximum  of the range of solution
		DOUBLE PRECISION :: pmf_rad_min   ! minimum  of the range of Prandtl-Meyer function [rad]
		DOUBLE PRECISION :: pmf_rad_mid   ! midpoint of the range of Prandtl-Meyer function [rad]
		DOUBLE PRECISION :: pmf_rad_max   ! maximum  of the range of Prandtl-Meyer function [rad]
		DOUBLE PRECISION :: pmf_rad_trg   ! Prandtl-Meyer function [rad] of the target

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

			IF( isTooBig ) STOP "angle for inverse Prandtl-Meyer function is too big!"

			! STEP.01.03
			! how about the allowable error
			IF ( PRESENT( err ) ) THEN
				err_allowable= err
			ELSE
				err_allowable= 1.0D+1 * EPSILON( err_allowable )
			ENDIF

		! STEP.02
		! calculate inverse function of Prandtl-Meyer function

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



MODULE SuperSonicNozzle

	! imported <MODULE>
	USE PrandtlMeyerCnst
	USE PrandtlMeyerNrml
	USE PrandtlMeyerInvs

	! Require all variables to be explicitly declared
	IMPLICIT NONE

	CONTAINS

	PURE FUNCTION MachAngleRad( mach )

		! argument for this <FUNCTION>
		DOUBLE PRECISION, INTENT(IN) :: mach ! mach number

		! return value of this <FUNCTION>
		DOUBLE PRECISION :: MachAngleRad

		MachAngleRad = ASIN( 1.0D+0/mach )
		RETURN

	END FUNCTION MachAngleRad



	PURE FUNCTION CalcCrossPoint( cdx, cdy, arg ) RESULT( CD_Crs )

		! argument for this <FUNCTION>
		DOUBLE PRECISION, DIMENSION(1:2), INTENT(IN) :: cdx ! x-coordinate of the cross point of line 1 and 2
		DOUBLE PRECISION, DIMENSION(1:2), INTENT(IN) :: cdy ! y-coordinate of the cross point of line 1 and 2
		DOUBLE PRECISION, DIMENSION(1:2), INTENT(IN) :: arg ! argument of line 1 and 2 [rad]

		! return value of this <FUNCTION>
		DOUBLE PRECISION, DIMENSION(1:2) :: CD_Crs ! coordinate of the cross point of line 1 and 2

		! local variables for this <FUNCTION>
		DOUBLE PRECISION, DIMENSION(1:2) :: Slp   ! Slp     of the line 1 and 2
		DOUBLE PRECISION, DIMENSION(1:2) :: intrcpt ! intercept of the line 1 and 2
		DOUBLE PRECISION                 :: buf

		! support variables for this <FUNCTION>
		INTEGER( KIND= 4 ) :: itr

		! STEP.01
		! calculate the Slp of line 1 and 2
		Slp(:) = TAN( arg(:) )

		! STEP.02
		! calculate the Slp of line 1 and 2
		FORALL ( itr=1:2:1 ) intrcpt(itr) = cdy(itr) - Slp(itr) * cdx(itr)

		! STEP.03
		! calculate the coordinate of the cross point
		buf          = 1.0D+0 / ( Slp(2) - Slp(1) )
		CD_Crs(1) = ( intrcpt(1) - intrcpt(2) ) * buf
		CD_Crs(2) = ( intrcpt(1)*Slp(2) - intrcpt(2)*Slp(1) ) * buf

		! STEP.END
		RETURN

	END FUNCTION CalcCrossPoint


END MODULE SuperSonicNozzle



PROGRAM MAIN

	! imported <MODULE>s
	USE PrandtlMeyerNrml
	USE PrandtlMeyerInvs
	USE SuperSonicNozzle
	USE support_IO

	! Require all variables to be explicitly declared
	IMPLICIT NONE



	! constants in this <PROGRAM>
	INTEGER( KIND= 4 ), PARAMETER :: UnitNum1 = 10
	INTEGER( KIND= 4 ), PARAMETER :: UnitNum2 = 11
	INTEGER( KIND= 4 ), PARAMETER :: UnitNum3 = 12

	CHARACTER( LEN= 63 ), PARAMETER :: path_fldr_save  = 
	CHARACTER( LEN= 13 ), PARAMETER :: path_file_save1 = 'TEST02_01.CSV'
	CHARACTER( LEN= 13 ), PARAMETER :: path_file_save2 = 'TEST02_02.CSV'
	CHARACTER( LEN= 13 ), PARAMETER :: path_file_save3 = 'TEST02_03.CSV'

	DOUBLE PRECISION, PARAMETER :: CDX_Crs_start = 0.0D+0 ! 
	DOUBLE PRECISION, PARAMETER :: CDY_Crs_start = 5.0D-1 !
	DOUBLE PRECISION, PARAMETER :: CDX_Div_end   = 0.0D+0 ! 
	DOUBLE PRECISION, PARAMETER :: CDY_Div_end   = 3.0D-1 !
	DOUBLE PRECISION, PARAMETER :: sphr          = 1.4D+0 ! specific heat ratio [-]
	DOUBLE PRECISION, PARAMETER :: MachNum_thrt  = 1.0D+0 ! Mach number at the throat of the Laval nozzle
	DOUBLE PRECISION, PARAMETER :: MachNum_otlt  = 2.0D+0 ! Mach number at the outlet of the Laval nozzle
	DOUBLE PRECISION, PARAMETER :: SlpWall_thrt  = 0.0D+0 ! Slp of the nozzle wall at the throat of the Laval nozzle [rad]
	DOUBLE PRECISION, PARAMETER :: SlpWall_otlt  = 0.0D+0 ! Slp of the nozzle wall at the outlet of the Laval nozzle [rad]

	INTEGER( KIND= 4 ), PARAMETER :: Num_MachLines_thrt = 10 ! the number of the Mach Lines at the throat



	! local variables in this <PROGRAM>
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: CD_Crs      ! coordinate of the cross point of Mach lines
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: CD_Div      ! coordinate of the dividing point of stream lines
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: RmnnInvrnts ! Riemann's conservation value
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: SlpCharLine ! Slp of the charastalic line

	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: MachAng_Area ! Mach angle at the each area defined by Mach lines [rad]
	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: MachNum_Area ! Mach number at the each area defined by Mach lines
	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: PM_func_Area ! Prandtl-Meyer function at the each area defined by Mach lines
	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: SlpMachLine  ! Slp of the Mach line [rad]
	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: VelcAng_Area ! angle the the velocity vectors at the each area defined by Mach lines

	DOUBLE PRECISION :: VelcAng_Area_max ! maximum of the angle the the velocity vectors at the each area defined by Mach lines

	INTEGER( KIND= 4 ) :: Num_Areas     ! the number of the areas defined by Mach lines
	INTEGER( KIND= 4 ) :: Num_CrsPnts   ! the number of total cross points of Mach lines
	INTEGER( KIND= 4 ) :: Num_DivPnts   ! the number of dividing points of stream lines
	INTEGER( KIND= 4 ) :: Num_MachLines ! the number of total Mach lines
	INTEGER( KIND= 4 ) :: itr
	INTEGER( KIND= 4 ) :: itr_Lv1 
	INTEGER( KIND= 4 ) :: itr_Lv2 



	! support variables for this <PROGRAM>
	INTEGER( KIND= 4 ) :: counter_Area, counter_CrsPnt
	INTEGER( KIND= 4 ) :: counter0, counter1, counter2, counter3, counter4, counter5, counterS, counterB
	INTEGER( KIND= 4 ) :: val_stat



	!--- MAIN PROCESS ---!

	! STEP.01
	! allocation
		Num_Areas     = ( Num_MachLines_thrt+1 )*( Num_MachLines_thrt+2 ) / 2
		Num_MachLines = ( Num_MachLines_thrt+1 )*  Num_MachLines_thrt
		Num_DivPnts   = 2*Num_MachLines_thrt+1

		PRINT '(A24,I)', 'number of divided areas', Num_Areas
		PRINT '(A24,I)', 'number of Mach lines',    Num_MachLines

		ALLOCATE(      CD_Crs(1:2,1:Num_Areas),   STAT= val_stat );      CD_Crs(:,:) = SQRT(-1.0D+0)
		ALLOCATE(      CD_Div(1:2,0:Num_DivPnts), STAT= val_stat );      CD_Div(:,:) = SQRT(-1.0D+0)
		ALLOCATE( RmnnInvrnts(1:2,1:Num_Areas),   STAT= val_stat ); RmnnInvrnts(:,:) = SQRT(-1.0D+0)
		ALLOCATE( SlpCharLine(1:2,1:Num_Areas),   STAT= val_stat ); SlpCharLine(:,:) = SQRT(-1.0D+0)

		ALLOCATE( MachAng_Area(1:Num_Areas),     STAT= val_stat ); MachAng_Area(:) = SQRT(-1.0D+0)
		ALLOCATE( MachNum_Area(1:Num_Areas),     STAT= val_stat ); MachNum_Area(:) = SQRT(-1.0D+0)
		ALLOCATE( PM_func_Area(1:Num_Areas),     STAT= val_stat ); PM_func_Area(:) = SQRT(-1.0D+0)
		ALLOCATE(  SlpMachLine(1:Num_MachLines), STAT= val_stat );  SlpMachLine(:) = SQRT(-1.0D+0)
		ALLOCATE( VelcAng_Area(1:Num_Areas),     STAT= val_stat ); VelcAng_Area(:) = SQRT(-1.0D+0)

		PRINT '(A)', 'allocations of arrays have finished.'
		! PRINT *, 'TEMPORARY STOP NOW'
		! PRINT *, 'WAITING USER INPUT'
		! READ *

	! STEP.02
	! calculation at the throat section of the nozzle

		MachNum_Area( 1 ) = MachNum_thrt                                ! Mach number [-]
		VelcAng_Area( 1 ) = SlpWall_thrt                                ! angle of velocity vector [rad]
		PM_func_Area( 1 ) = PMfuncRad( MACH= MachNum_thrt, SPHR= sphr ) ! Prandtl-Meyer function [rad]
		MachAng_Area( 1 ) = MachAngleRad( MachNum_thrt )                ! Mach angle [rad]

		RmnnInvrnts( 1, 1 ) = PM_func_Area( 1 ) - VelcAng_Area( 1 ) ! Riemann invariant, left  [rad]
		RmnnInvrnts( 2, 1 ) = PM_func_Area( 1 ) + VelcAng_Area( 1 ) ! Riemann invariant, right [rad]

		SlpCharLine( 1, 1 ) = VelcAng_Area( 1 ) + MachAng_Area( 1 ) ! slope of charastalic line, left  [rad]
		SlpCharLine( 2, 1 ) = VelcAng_Area( 1 ) - MachAng_Area( 1 ) ! slope of charastalic line, right [rad]

	! STEP.03
	! calculation at the outlet of the nozzle

		MachNum_Area( Num_Areas ) = MachNum_otlt                                ! Mach number [-]
		VelcAng_Area( Num_Areas ) = SlpWall_otlt                                ! angle of velocity vector [rad]
		PM_func_Area( Num_Areas ) = PMfuncRad( MACH= MachNum_otlt, SPHR= sphr ) ! Prandtl-Meyer function [rad]
		MachAng_Area( Num_Areas ) = MachAngleRad( MachNum_otlt )                ! Mach angle [rad]

		RmnnInvrnts( 1, Num_Areas ) = PM_func_Area( Num_Areas ) - VelcAng_Area( Num_Areas ) ! Riemann invariant, left  [rad]
		RmnnInvrnts( 2, Num_Areas ) = PM_func_Area( Num_Areas ) + VelcAng_Area( Num_Areas ) ! Riemann invariant, right [rad]

		SlpCharLine( 1, Num_Areas ) = VelcAng_Area( Num_Areas ) + MachAng_Area( Num_Areas ) ! slope of charastalic line, left  [rad]
		SlpCharLine( 2, Num_Areas ) = VelcAng_Area( Num_Areas ) - MachAng_Area( Num_Areas ) ! slope of charastalic line, right [rad]

	! STEP.04
	! calculation at Area No.2 to No.`N`+1
	! `N` is the number of Mach line at the outlet of throat

		! STEP.04.01
		! calculation at Area No.`N`+1

			itr = Num_MachLines_thrt + 1 ! set the target iterator

			RmnnInvrnts( 1, itr ) = RmnnInvrnts( 1, 1         ) ! Riemann invariant, left  [rad]
			RmnnInvrnts( 2, itr ) = RmnnInvrnts( 2, Num_Areas ) ! Riemann invariant, right [rad]

			PM_func_Area( itr ) = 5.0D-1 * ( RmnnInvrnts( 2, itr ) + RmnnInvrnts( 1, itr ) ) ! Prandtl-Meyer function [rad]
			VelcAng_Area( itr ) = 5.0D-1 * ( RmnnInvrnts( 2, itr ) - RmnnInvrnts( 1, itr ) ) ! angle of velocity vector [rad]
			MachNum_Area( itr ) = iPMfuncBisection( RAD= PM_func_Area( itr ), SPHR= sphr )   ! Mach number [-]
			MachAng_Area( itr ) = MachAngleRad( MachNum_Area( itr ) )                        ! Mach angle [rad]

		! STEP.04.02
		! calculation at Area No.2 to No.`N`

			! angle of velocity vector [rad]
			VelcAng_Area_max= VelcAng_Area( Num_MachLines_thrt + 1 )

			IF( VelcAng_Area_max .EQ. 0.0D+0 ) THEN
				FORALL ( itr= 2:Num_MachLines_thrt:1 ) VelcAng_Area( itr ) = 0.0D+0
			ELSE
				FORALL ( itr= 2:Num_MachLines_thrt:1 ) VelcAng_Area( itr ) = VelcAng_Area_max / REAL( Num_MachLines_thrt * (itr-1), KIND= KIND( 1.0D+0 ) )
			ENDIF

			! Prandtl-Meyer function [rad]
			! using right charastalic line from Area No.1
			FORALL ( itr= 2:Num_MachLines_thrt:1 ) PM_func_Area( itr ) = RmnnInvrnts( 2, 1 ) + VelcAng_Area( itr )

			! Riemann invariant
			FORALL ( itr= 2:Num_MachLines_thrt:1 )
				RmnnInvrnts( 1, itr ) = VelcAng_Area( itr ) - PM_func_Area( itr ) ! Riemann invariant, left  [rad]
				RmnnInvrnts( 2, itr ) = VelcAng_Area( itr ) + PM_func_Area( itr ) ! Riemann invariant, right [rad]
			END FORALL

			! Mach number [-]
			DO itr = 2, Num_MachLines_thrt, 1
				MachNum_Area( itr ) = iPMfuncBisection( RAD= PM_func_Area( itr ), SPHR= sphr )
			ENDDO

			! Mach angle [rad]
			FORALL ( itr= 2:Num_MachLines_thrt:1 ) MachAng_Area( itr ) = MachAngleRad( MachNum_Area( itr ) )

	! STEP.05
	! calculation at Area No.`N`+2 to No.`N_Area`+1
	! `N`      is the number of Mach line at the outlet of throat
	! `N_Area` is the number of the areas defined by Mach lines

		! STEP.05.01
		! initialization
		counter1 = 1
		counter2 = 0

		! STEP.05.02
		DO itr_Lv1 = Num_MachLines_thrt+1, 1, -1

			! STEP.05.02.01
			! update the counter
			counter1 = counter1 + itr_Lv1
			IF( counter1 .GT. Num_Areas ) EXIT
			counter2 = counter1

			! STEP.05.02.03
			! calculation on the center line of the nozzle
			VelcAng_Area( counter1 ) = 0.0D+0                                                        ! angle of velocity vector [rad]
			PM_func_Area( counter1 ) = RmnnInvrnts( 2, counter1 - itr_Lv1 + 1 )                      ! Prandtl-Meyer function [rad]
			MachNum_Area( counter1 ) = iPMfuncBisection( RAD= PM_func_Area( counter1 ), SPHR= sphr ) ! Mach number [-]
			MachAng_Area( counter1 ) = MachAngleRad( MachNum_Area( counter1 ) )                      ! Mach angle [rad]

			RmnnInvrnts( 1, counter1 ) = PM_func_Area( counter1 ) - VelcAng_Area( counter1 ) ! Riemann invariant, left  [rad]
			RmnnInvrnts( 2, counter1 ) = PM_func_Area( counter1 - itr_Lv1 + 1 )              ! Riemann invariant, right [rad]

			! STEP.05.02.03
			! calculation on the wall of the nozzle
			DO itr_Lv2 = 1, itr_Lv1-2, 1

				counter2 = counter2 + 1
				counter3 = counter2 - itr_Lv1 + 1
				counter4 = counter2 - 1

				RmnnInvrnts( 1, counter2 ) = RmnnInvrnts( 1, counter4 ) ! Riemann invariant, left  [rad]
				RmnnInvrnts( 2, counter2 ) = RmnnInvrnts( 2, counter3 ) ! Riemann invariant, right [rad]

				VelcAng_Area( counter2 ) = 5.0D-1 * ( RmnnInvrnts( 2, counter2 ) - RmnnInvrnts( 1, counter2 ) ) ! angle of velocity vector [rad]
				PM_func_Area( counter2 ) = 5.0D-1 * ( RmnnInvrnts( 2, counter2 ) + RmnnInvrnts( 1, counter2 ) ) ! Prandtl-Meyer function [rad]
				MachNum_Area( counter2 ) = iPMfuncBisection( RAD= PM_func_Area( counter2 ), SPHR= sphr )        ! Mach number [-]
				MachAng_Area( counter2 ) = MachAngleRad( MachNum_Area( counter2 ) )                             ! Mach angle [rad]

			ENDDO

		ENDDO

	! STEP.06
	! calculation the angles between `the center line of the nozzle` and `charastalic line`
	
		FORALL ( itr = 2:Num_Areas-1:1 )
			SlpCharLine( 1, itr ) = VelcAng_Area( itr ) + MachAng_Area( itr ) ! between `the center line of the nozzle` and `LEFT  charastalic line`
			SlpCharLine( 2, itr ) = VelcAng_Area( itr ) - MachAng_Area( itr ) ! between `the center line of the nozzle` and `RIGHT charastalic line`
		END FORALL

	! STEP.07
	! calculation the Slp of the Mach lines at the each area defined by Mach lines

		! STEP.07.01
		! initialization of the counters
		counter1 = 0
		counter4 = 0
		counter5 = Num_MachLines_thrt - 1

		! STEP.07.02
		! calculation the slope of the Mach lines
		DO itr_Lv1= Num_MachLines_thrt, 1, -1

			! using RIGHT charastalic lines
			DO itr_Lv2= 1, itr_Lv1, 1

				! update the counters
				counter1 = counter1 + 1
				counter2 = counter1 - counter4 
				counter3 = counter2 + 1

				! calculate the slope of the target Mach line
				SlpMachLine( counter1 ) = 5.0D-1 * ( SlpCharLine( 2, counter2 ) + SlpCharLine( 2, counter3 ) )

			ENDDO

			! update the counters
			counter4 = counter4 + itr_Lv1 - 1

			! using LEFT charastalic lines
			DO itr_Lv2= 1, itr_Lv1, 1

				! update the counters
				counter1 = counter1 + 1
				counter2 = counter1 - counter5
				counter3 = counter2 + itr_Lv1

				! calculate the slope of the target Mach line
				SlpMachLine( counter1 ) = 5.0D-1 * ( SlpCharLine( 1, counter2 ) + SlpCharLine( 1, counter3 ) )

			ENDDO

			! update the counters
			counter5 = counter5 + itr_Lv1 - 2

		ENDDO

	! STEP.08
	! calculation of the cross points of the Mach lines

		! STEP.08.01
		! about No.1 cross point
		CD_Crs( 1:2, 1 ) = (/ CDX_Crs_start, CDY_Crs_start /)

		! STEP.08.02
		! about No.2 cross point
		CD_Crs( 1:2, 2 ) = CalcCrossPoint(       & 
			CDX= (/ 0.0D+0, CD_Crs(1,1)      /), & 
			CDY= (/ 0.0D+0, CD_Crs(2,1)      /), & 
			ARG= (/ 0.0D+0, SlpMachLine( 1 ) /)  & 
		)

		! STEP.08.03
		! from No.3 to No.`N`+1
		! `N` is the number of Mach line at the outlet of throat
		DO itr = 3, Num_MachLines_thrt+1, 1
			CD_Crs( 1:2, itr ) = CalcCrossPoint( &
				CDX= (/ CD_Crs(1,1),          CD_Crs(1,itr-1)                         /), & 
				CDY= (/ CD_Crs(2,1),          CD_Crs(2,itr-1)                         /), & 
				ARG= (/ SlpMachLine( itr-1 ), SlpMachLine( Num_MachLines_thrt-2+itr ) /)  &
			)
		ENDDO

		! STEP.08.04
		! about No.`N`+2
		counter1 = Num_MachLines_thrt + 1
		counter2 = Num_MachLines_thrt * 2

		CD_Crs( 1:2, Num_MachLines_thrt+2 ) = CalcCrossPoint( &
			CDX= (/ CD_Crs(1,1),             CD_Crs(1,counter1)      /), & 
			CDY= (/ CD_Crs(2,1),             CD_Crs(2,counter1)      /), & 
			ARG= (/ SlpMachLine( counter1 ), SlpMachLine( counter2 ) /)  &
		)

		! STEP.08.05
		! from No.`N`+3 to the last
		counter0 = Num_MachLines_thrt + 3
		counter1 = 2
		counter3 = Num_MachLines_thrt + Num_MachLines_thrt + 1

		DO itr_Lv1 = 2, Num_MachLines_thrt, 1

			! on the center line of the nozzle
			counter0 = counter0 + 1
			counter1 = counter1 + 1
			counter2 = counter1 + ( Num_MachLines_thrt - itr_Lv1 + 1 )
			counter4 = counter3 + ( Num_MachLines_thrt - itr_Lv1 )

			CD_Crs( 1:2, counter0 ) = CalcCrossPoint( &
					CDX= (/ CD_Crs( 1, counter1 ), 0.0D+0 /), & 
					CDY= (/ CD_Crs( 2, counter1 ), 0.0D+0 /), & 
					ARG= (/ SlpMachLine( counter3 ), 0.0D+0 /) &
				)

			! Neither on the center line nor on the wall of nozzle
			DO itr_Lv2 = 2, Num_MachLines_thrt-itr_Lv1, 1

				counter0 = counter0 + 1
				counter1 = counter1 + 1
				counter2 = counter1 + ( Num_MachLines_thrt - itr_Lv1 + 1 )
				counter3 = counter3 + 1
				counter4 = counter3 + ( Num_MachLines_thrt - itr_Lv1 )

				CD_Crs( 1:2, counter0 ) = CalcCrossPoint( &
						CDX= (/ CD_Crs( 1, counter1 ), CD_Crs( 1, counter2 ) /), & 
						CDY= (/ CD_Crs( 2, counter1 ), CD_Crs( 2, counter2 ) /), & 
						ARG= (/ SlpMachLine( counter3 ), SlpMachLine( counter4 ) /) &
					)
			ENDDO

			! on the wall of nozzle
			counter0 = counter0 + 1
			counter1 = counter1 + 1
			counter2 = counter1 + ( Num_MachLines_thrt - itr_Lv1 + 1 )
			counter3 = counter3 + 1
			counter4 = counter3 + ( Num_MachLines_thrt - itr_Lv1 )
			counter3 = counter2

			CD_Crs( 1:2, counter0 ) = CalcCrossPoint( &
					CDX= (/ CD_Crs( 1, counter1 ), CD_Crs( 1, counter2 ) /), & 
					CDY= (/ CD_Crs( 2, counter1 ), CD_Crs( 2, counter2 ) /), & 
					ARG= (/ SlpMachLine( counter3 ), SlpMachLine( counter4 ) /) &
				)

			! update the conters
			counter1 = counter1 + 1
			counter3 = counter4 + 1

		ENDDO

	! STEP.09
	! calculation of the dividing points of the stream lines

		! STEP.01
		! initialization
		CD_Div( 1:2, 0 ) = (/ CDX_Div_end, CDY_Div_end /)

		! STEP.02
		FORALL ( itr= 1:Num_MachLines_thrt:1 ) &
			CD_Div( 1:2, itr ) = CalcCrossPoint( & 
				CDX= (/ CD_Crs( 1, 1 ),      CD_Div( 1, itr-1 )  /), & 
				CDY= (/ CD_Crs( 1, 1 ),      CD_Div( 2, itr-1 )  /), & 
				ARG= (/ SlpMachLine( itr ),  VelcAng_Area( itr ) /)  & 
			)

		! STEP.03
		! on the wall of nozzle
		counter0 = Num_MachLines_thrt + 1
		counter1 = Num_MachLines_thrt * 2
		counter2 = Num_MachLines_thrt + Num_MachLines_thrt

		DO itr= Num_MachLines_thrt+1, Num_DivPnts-1, 1

			CD_Div( 1:2, itr ) = CalcCrossPoint( & 
				CDX= (/ CD_Div( 1, itr-1 ),       CD_Crs( 1, counter0 )   /), & 
				CDY= (/ CD_Div( 1, itr-1 ),       CD_Crs( 2, counter0 )   /), & 
				ARG= (/ VelcAng_Area( counter0 ), SlpMachLine( counter1 ) /) & 
			)

			counter0 = counter0 + Num_DivPnts - itr
			counter1 = counter1 + 2*(Num_DivPnts-itr-1)

		ENDDO



	! STEP.10
	OPEN( UNIT= UnitNum1, DEFAULTFILE= path_fldr_save, FILE= path_file_save1, IOSTAT= val_stat, ACTION= 'WRITE', STATUS= 'REPLACE' )
	CALL CheckIostatOpen( VAL= val_stat, PATH= path_fldr_save // '\' // path_file_save1 )

	OPEN( UNIT= UnitNum2, DEFAULTFILE= path_fldr_save, FILE= path_file_save2, IOSTAT= val_stat, ACTION= 'WRITE', STATUS= 'REPLACE' )
	CALL CheckIostatOpen( VAL= val_stat, PATH= path_fldr_save // '\' // path_file_save2 )

	OPEN( UNIT= UnitNum3, DEFAULTFILE= path_fldr_save, FILE= path_file_save3, IOSTAT= val_stat, ACTION= 'WRITE', STATUS= 'REPLACE' )
	CALL CheckIostatOpen( VAL= val_stat, PATH= path_fldr_save // '\' // path_file_save3 )

	WRITE( UNIT= UnitNum1, FMT='(A)', ADVANCE='YES', IOSTAT= val_stat ) & 
		'ITR'                                // ',' // &
		'REIMANN CONSERVATION VALUE (LEFT)'  // ',' // &
		'REIMANN CONSERVATION VALUE (RIGHT)' // ',' // &
		'SLOPE (CHARASTALIC LINE LEFT)'      // ',' // &
		'SLOPE (CHARASTALIC LINE RIGHT)'     // ',' // &
		'MACH ANGLE'                         // ',' // &
		'MACH NUMBER'                        // ',' // &
		'PM FUNCTION'                        // ',' // &
		'SLOPE (MACH LINE)'                  // ',' // &
		'ANGLE OF VELOCITY VECTOR'

	DO itr = 1, Num_Areas, 1
		WRITE( UNIT= UnitNum1, FMT='(I5,11(","ES23.15e3))', ADVANCE='YES', IOSTAT= val_stat ) &
			itr, &
			RmnnInvrnts(1,itr), RmnnInvrnts(2,itr), &
			SlpCharLine(1,itr), SlpCharLine(2,itr), &
			MachAng_Area(itr), &
			MachNum_Area(itr), &
			PM_func_Area(itr), &
			SlpMachLine(itr), &
			VelcAng_Area(itr)
	ENDDO

	WRITE( UNIT= UnitNum2, FMT='(A)', ADVANCE='YES', IOSTAT= val_stat ) 'ITR,CDX,CDY'

	DO itr = LBOUND( CD_Crs, DIM=2 ), UBOUND( CD_Crs, DIM=2 ), 1
		WRITE( UNIT= UnitNum2, FMT='(I5,2(","ES23.15e3))', ADVANCE='YES', IOSTAT= val_stat ) &
			itr, CD_Crs(1,itr), CD_Crs(2,itr)
	ENDDO

	WRITE( UNIT= UnitNum3, FMT='(A)', ADVANCE='YES', IOSTAT= val_stat ) 'ITR,CDX,CDY'

	DO itr = LBOUND( CD_Div, DIM=2 ), UBOUND( CD_Div, DIM=2 ), 1
		WRITE( UNIT= UnitNum3, FMT='(I5,2(","ES23.15e3))', ADVANCE='YES', IOSTAT= val_stat ) &
			itr, CD_Div(1,itr), CD_Div(2,itr)
	ENDDO

	! STEP.13
	CLOSE( UNIT= UnitNum1, STATUS= 'KEEP', IOSTAT= val_stat )
	CALL CheckIostatClose( VAL= val_stat, PATH= path_fldr_save // '\' // path_file_save1 )

	CLOSE( UNIT= UnitNum2, STATUS= 'KEEP', IOSTAT= val_stat )
	CALL CheckIostatClose( VAL= val_stat, PATH= path_fldr_save // '\' // path_file_save2 )

	CLOSE( UNIT= UnitNum3, STATUS= 'KEEP', IOSTAT= val_stat )
	CALL CheckIostatClose( VAL= val_stat, PATH= path_fldr_save // '\' // path_file_save3 )

	! STEP.14
	! deallocation
	DEALLOCATE(       CD_Crs, STAT= val_stat )
	DEALLOCATE(       CD_Div, STAT= val_stat )
	DEALLOCATE(  RmnnInvrnts, STAT= val_stat )
	DEALLOCATE(  SlpCharLine, STAT= val_stat )
	DEALLOCATE( VelcAng_Area, STAT= val_stat )
	DEALLOCATE( MachNum_Area, STAT= val_stat )
	DEALLOCATE( PM_func_Area, STAT= val_stat )
	DEALLOCATE(  SlpMachLine, STAT= val_stat )

	! STEP.END
	PRINT *, "ALL OVER"

END PROGRAM MAIN