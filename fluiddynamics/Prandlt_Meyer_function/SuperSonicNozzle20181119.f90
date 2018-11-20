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



	FUNCTION CalcCrossPoint( cdx, cdy, arg ) RESULT( CD_Crs )

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

	! Require all variables to be explicitly declared
	IMPLICIT NONE



	! constants in this <PROGRAM>
	DOUBLE PRECISION, PARAMETER :: CDX_Crs_start = 0.0D+0 ! 
	DOUBLE PRECISION, PARAMETER :: CDY_Crs_start = 5.0D-1 !
	DOUBLE PRECISION, PARAMETER :: CDX_Div_end   = 0.0D+0 ! 
	DOUBLE PRECISION, PARAMETER :: CDY_Div_end   = 3.0D-1 !
	DOUBLE PRECISION, PARAMETER :: sphr          = 1.4D+0 ! specific heat ratio [-]
	DOUBLE PRECISION, PARAMETER :: MachNum_thrt  = 1.0D+0 ! Mach number at the throat of the Laval nozzle
	DOUBLE PRECISION, PARAMETER :: MachNum_otlt  = 2.0D+0 ! Mach number at the outlet of the Laval nozzle
	DOUBLE PRECISION, PARAMETER :: SlpWall_thrt  = 0.0D+0 ! Slp of the nozzle wall at the throat of the Laval nozzle [rad]
	DOUBLE PRECISION, PARAMETER :: SlpWall_otlt  = 0.0D+0 ! Slp of the nozzle wall at the outlet of the Laval nozzle [rad]

	INTEGER( KIND= 4 ), PARAMETER :: Num_MachLines_thrt = 100 ! the number of the Mach Lines at the throat



	! local variables in this <PROGRAM>
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: CD_Crs      ! coordinate of the cross point of Mach lines
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: CD_Div      ! coordinate of the dividing point of stream lines
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: RmnnCnsvVal ! Riemann's conservation value
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: SlpCharLine ! Slp of the charastalic line

	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: MachAng_Area ! Mach angle at the each area defined by Mach lines [rad]
	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: MachNum_Area ! Mach number at the each area defined by Mach lines
	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: PM_func_Area ! Prandtl-Meyer function at the each area defined by Mach lines
	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: SlpMachLine  ! Slp of the Mach line [rad]
	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: SlpWallArea  ! Slp of the nozzle wall [rad]
	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: VelcAng_Area ! angle the the velocity vectors at the each area defined by Mach lines

	INTEGER( KIND= 4 ) :: Num_Areas     ! the number of the areas defined by Mach lines
	INTEGER( KIND= 4 ) :: Num_CrsPnts   ! the number of total cross points of Mach lines
	INTEGER( KIND= 4 ) :: Num_DivPnts   ! the number of dividing points of stream lines
	INTEGER( KIND= 4 ) :: Num_MachLines ! the number of total Mach lines
	INTEGER( KIND= 4 ) :: itr
	INTEGER( KIND= 4 ) :: itr_AreaLv1 
	INTEGER( KIND= 4 ) :: itr_AreaLv2 



	! support variables for this <PROGRAM>
	INTEGER( KIND= 4 ) :: counter_Area, counter_CrsPnt
	INTEGER( KIND= 4 ) :: counter0, counter1, counter2, counter3, counter4, counter5, counterS, counterB
	INTEGER( KIND= 4 ) :: val_stat



	!--- MAIN PROCESS ---!

	! STEP.01
	! allocation
		Num_Areas     = ( Num_MachLines_thrt+1 )*( Num_MachLines_thrt+2 ) / 2
		Num_CrsPnts   = ( Num_MachLines_thrt+1 )*  Num_MachLines_thrt / 2
		Num_DivPnts   = Num_MachLines_thrt * 2
		Num_MachLines = ( Num_MachLines_thrt+1 ) * Num_MachLines_thrt

		PRINT '(A24,I)', 'number of divided areas', Num_Areas
		PRINT '(A24,I)', 'number of Mach lines',    Num_MachLines

		ALLOCATE(      CD_Crs(1:2,1:Num_CrsPnts), STAT= val_stat )
		ALLOCATE(      CD_Div(1:2,1:Num_DivPnts), STAT= val_stat )
		ALLOCATE( RmnnCnsvVal(1:2,1:Num_Areas),   STAT= val_stat )
		ALLOCATE( SlpCharLine(1:2,1:Num_Areas),   STAT= val_stat )

		ALLOCATE( MachAng_Area(1:Num_Areas), STAT= val_stat )
		ALLOCATE( MachNum_Area(1:Num_Areas), STAT= val_stat )
		ALLOCATE( PM_func_Area(1:Num_Areas), STAT= val_stat )
		ALLOCATE(  SlpMachLine(1:Num_Areas), STAT= val_stat )
		ALLOCATE(  SlpWallArea(1:Num_Areas), STAT= val_stat )
		ALLOCATE( VelcAng_Area(1:Num_Areas), STAT= val_stat )

		PRINT '(A)', 'allocations of arrays have finished.'
		! PRINT *, 'TEMPORARY STOP NOW'
		! PRINT *, 'WAITING USER INPUT'
		! READ *

	! STEP.02
	! calculation at the throat section of the nozzle
		MachNum_Area( 1 ) = MachNum_thrt
		VelcAng_Area( 1 ) = SlpWall_thrt
		PM_func_Area( 1 ) = PMfuncRad( MACH= MachNum_Area( 1 ), SPHR= sphr )
		MachAng_Area( 1 ) = MachAngleRad( MachNum_Area( 1 ) )

		RmnnCnsvVal( 1, 1 ) = PM_func_Area( 1 ) - MachAng_Area( 1 ) ! Riemann left 
		RmnnCnsvVal( 2, 1 ) = PM_func_Area( 1 ) + MachAng_Area( 1 ) ! Riemann right
		SlpCharLine( 1, 1 ) = VelcAng_Area( 1 ) + MachAng_Area( 1 ) ! left  charastalic
		SlpCharLine( 2, 1 ) = VelcAng_Area( 1 ) - MachAng_Area( 1 ) ! right charastalic

	! STEP.03
	! calculation at the outlet of the nozzle
		MachNum_Area( Num_Areas ) = MachNum_otlt
		VelcAng_Area( Num_Areas ) = SlpWall_otlt
		PM_func_Area( Num_Areas ) = PMfuncRad( MACH= MachNum_otlt, SPHR= sphr )
		MachAng_Area( Num_Areas ) = MachAngleRad( MachNum_otlt )

		RmnnCnsvVal( 1, Num_Areas ) = PM_func_Area( Num_Areas ) - MachAng_Area( Num_Areas ) ! Riemann left 
		RmnnCnsvVal( 2, Num_Areas ) = PM_func_Area( Num_Areas ) + MachAng_Area( Num_Areas ) ! Riemann right
		SlpCharLine( 1, Num_Areas ) = VelcAng_Area( Num_Areas ) + MachAng_Area( Num_Areas ) ! left  charastalic
		SlpCharLine( 2, Num_Areas ) = VelcAng_Area( Num_Areas ) - MachAng_Area( Num_Areas ) ! right charastalic

	! STEP.04
	! calculation at Area No.2 to No.`N`+1
	! `N` is the number of Mach line at the outlet of throat

		! STEP.04.01
		! calculation at Area No.`N`+1
		itr                   = Num_MachLines_thrt + 1
		RmnnCnsvVal( 1, itr ) = RmnnCnsvVal( 1, 1         ) ! Riemann left 
		RmnnCnsvVal( 2, itr ) = RmnnCnsvVal( 2, Num_Areas ) ! Riemann right
		PM_func_Area(   itr ) = 5.0D-1 * ( RmnnCnsvVal( 2, itr ) + RmnnCnsvVal( 1, itr ) )
		VelcAng_Area(   itr ) = 5.0D-1 * ( RmnnCnsvVal( 2, itr ) - RmnnCnsvVal( 1, itr ) )
		MachNum_Area(   itr ) = iPMfuncBisection( RAD= PM_func_Area( itr ), SPHR= sphr )
		MachAng_Area(   itr ) = MachAngleRad( MachNum_Area( itr ) )

		! STEP.04.02
		! calculation at Area No.2 to No.`N`
		FORALL ( itr= 2:Num_MachLines_thrt:1 ) VelcAng_Area( itr ) = VelcAng_Area( Num_MachLines_thrt + 1 ) / REAL( Num_MachLines_thrt * itr, KIND= KIND( 1.0D+0 ) )
		FORALL ( itr= 2:Num_MachLines_thrt:1 ) PM_func_Area( itr ) = RmnnCnsvVal( 2, 1 ) + VelcAng_Area( itr )

		FORALL ( itr= 2:Num_MachLines_thrt:1 ) RmnnCnsvVal( 1, itr ) = VelcAng_Area( itr ) - PM_func_Area( itr ) ! Riemann left 
		FORALL ( itr= 2:Num_MachLines_thrt:1 ) RmnnCnsvVal( 2, itr ) = VelcAng_Area( itr ) + PM_func_Area( itr ) ! Riemann right

		DO itr = 2, Num_MachLines_thrt, 1
			MachNum_Area( itr ) = iPMfuncBisection( RAD= PM_func_Area( itr ), SPHR= sphr )
		ENDDO

		FORALL ( itr= 2:Num_MachLines_thrt:1 ) MachAng_Area( itr ) = MachAngleRad( MachNum_Area( itr ) )

	! STEP.05
	! calculation at Area No.`N`+2 to No.`N_Area`+1
	! `N`      is the number of Mach line at the outlet of throat
	! `N_Area` is the number of the areas defined by Mach lines

		! STEP.05.01
		! initialization
		counter_Area = 1

		! STEP.05.02
		DO itr_AreaLv1 = Num_MachLines_thrt+1, 1, -1

			! STEP.05.02.01
			! update the counter
			counter_Area = counter_Area+1
			IF( counter_Area .GT. Num_Areas ) EXIT

			! STEP.05.02.03
			! calculation on the center line of the nozzle
			VelcAng_Area(   counter_Area ) = 0.0D+0
			PM_func_Area(    counter_Area ) = RmnnCnsvVal( 2, counter_Area-(itr_AreaLv1-1) )
			RmnnCnsvVal( 1, counter_Area ) = PM_func_Area( counter_Area ) ! Riemann left
			RmnnCnsvVal( 2, counter_Area ) = PM_func_Area( counter_Area ) ! Riemann right
			MachNum_Area(   counter_Area ) = iPMfuncBisection( RAD= PM_func_Area( counter_Area ), SPHR= sphr )
			MachAng_Area(   counter_Area ) = MachAngleRad( MachNum_Area( counter_Area ) )

			! STEP.05.02.03
			! calculation on the wall of the nozzle
			DO itr_AreaLv2 = 1, itr_AreaLv1-2, 1

				RmnnCnsvVal( 1, counter_Area+1 ) = RmnnCnsvVal( 1, counter_Area               ) ! Riemann left
				RmnnCnsvVal( 2, counter_Area+1 ) = RmnnCnsvVal( 2, counter_Area+2-itr_AreaLv2 ) ! Riemann right
				PM_func_Area(    counter_Area+1 ) = 5.0D-1 * ( RmnnCnsvVal( 1, counter_Area+1 ) + RmnnCnsvVal( 2, counter_Area+1 ) )
				VelcAng_Area(   counter_Area+1 ) = 5.0D-1 * ( RmnnCnsvVal( 1, counter_Area+1 ) - RmnnCnsvVal( 2, counter_Area+1 ) )
				MachNum_Area(   counter_Area+1 ) = iPMfuncBisection( RAD= PM_func_Area( counter_Area+1 ), SPHR= sphr )
				MachAng_Area(   counter_Area+1 ) = MachAngleRad( MachNum_Area( counter_Area+1 ) )

			ENDDO


		ENDDO

	! STEP.06
	! calculation the angles between `the center line of the nozzle` and `charastalic line`
		DO itr_AreaLv1 = 2, Num_Areas-1, 1
			SlpCharLine( 1, itr_AreaLv1 ) = VelcAng_Area( itr_AreaLv1 ) + MachAng_Area( itr_AreaLv1 ) ! between `the center line of the nozzle` and `LEFT  charastalic line`
			SlpCharLine( 2, itr_AreaLv1 ) = VelcAng_Area( itr_AreaLv1 ) - MachAng_Area( itr_AreaLv1 ) ! between `the center line of the nozzle` and `RIGHT charastalic line`
		ENDDO

	! STEP.07
	! calculation the Slp of the Mach lines at the each area defined by Mach lines

		! STEP.07.01
		! initialization of the counters
		counter1 = 0
		counter4 = 0
		counter5 = Num_MachLines_thrt - 1

		! STEP.07.02
		! calculation the Slp of the Mach lines
		DO itr_AreaLv1= Num_MachLines_thrt, 1, -1

			! using RIGHT charastalic lines
			DO itr_AreaLv2= 1, itr_AreaLv1, 1

				! update the counters
				counter1 = counter1 + 1
				counterS = counter1 - counter4 
				counterB = counterS + 1

				! calculate the Slp of the target Mach line
				SlpMachLine( counter1 ) = 5.0D-1 * ( SlpCharLine( 2, counterS ) + SlpCharLine( 2, counterB ) )

			ENDDO

			! update the counters
			counter4 = counter4 + itr_AreaLv1 - 1

			! using LEFT charastalic lines
			DO itr_AreaLv2= 1, itr_AreaLv1, 1

				! update the counters
				counter1 = counter1 + 1
				counterS = counter1 - counter5
				counterB = counterS + itr_AreaLv1

				! calculate the Slp of the target Mach line
				SlpMachLine( counter1 ) = 5.0D-1 * ( SlpCharLine( 1, counterS ) + SlpCharLine( 1, counterB ) )

			ENDDO

			! update the counters
			counter5 = counter5 + itr_AreaLv1 - 2

		ENDDO

	! STEP.08
	! calculation of the cross points of the Mach lines

		! STEP.08.01
		! about No.1 cross point
		CD_Crs( 1:2, 1 ) = (/ CDX_Crs_start, CDY_Crs_start /)
		! STEP.08.02
		! about No.2 cross point
		CD_Crs( 1:2, 2 ) = CalcCrossPoint( & 
			CDX= (/ 0.0D+0, CDX_Crs_start /), & 
			CDY= (/ 0.0D+0, CDY_Crs_start /), & 
			ARG= (/ 0.0D+0, SlpMachLine( 1 ) /) & 
		)

		! STEP.08.03
		! from No.3 to No.`N`+1
		! `N` is the number of Mach line at the outlet of throat
		DO itr = 3, Num_MachLines_thrt+1, 1
			CD_Crs( 1:2, itr ) = CalcCrossPoint( &
				CDX= (/ CD_Crs(1,1), CD_Crs(1,itr-1) /), & 
				CDY= (/ CD_Crs(2,1), CD_Crs(2,itr-1) /), & 
				ARG= (/ SlpMachLine( itr-1 ), SlpMachLine( Num_MachLines_thrt-2+itr ) /) &
			)
		ENDDO

		! STEP.08.04
		! about No.`N`+2
		counter1 = Num_MachLines_thrt + 1
		counter2 = Num_MachLines_thrt + Num_MachLines_thrt

		CD_Crs( 1:2, Num_MachLines_thrt+2 ) = CalcCrossPoint( &
			CDX= (/ CD_Crs(1,1), CD_Crs(1,counter1) /), & 
			CDY= (/ CD_Crs(2,1), CD_Crs(2,counter1) /), & 
			ARG= (/ SlpMachLine( counter1 ), SlpMachLine( counter2 ) /) &
		)

		! STEP.08.05
		! from No.`N`+3 to the last
		counter0 = Num_MachLines_thrt + 3
		counter1 = 2
		counter3 = Num_MachLines_thrt + Num_MachLines_thrt + 1

		DO itr_AreaLv1 = 2, Num_MachLines_thrt, 1

			! on the center line of the nozzle
			counter0 = counter0 + 1
			counter1 = counter1 + 1
			counter2 = counter1 + ( Num_MachLines_thrt - itr_AreaLv1 + 1 )
			counter4 = counter3 + ( Num_MachLines_thrt - itr_AreaLv1 )

			CD_Crs( 1:2, counter0 ) = CalcCrossPoint( &
					CDX= (/ CD_Crs( 1, counter1 ), 0.0D+0 /), & 
					CDY= (/ CD_Crs( 2, counter1 ), 0.0D+0 /), & 
					ARG= (/ SlpMachLine( counter3 ), 0.0D+0 /) &
				)

			! Neither on the center line nor on the wall of nozzle
			DO itr_AreaLv2 = 2, Num_MachLines_thrt-itr_AreaLv1, 1

				counter0 = counter0 + 1
				counter1 = counter1 + 1
				counter2 = counter1 + ( Num_MachLines_thrt - itr_AreaLv1 + 1 )
				counter3 = counter3 + 1
				counter4 = counter3 + ( Num_MachLines_thrt - itr_AreaLv1 )

				CD_Crs( 1:2, counter0 ) = CalcCrossPoint( &
						CDX= (/ CD_Crs( 1, counter1 ), CD_Crs( 1, counter2 ) /), & 
						CDY= (/ CD_Crs( 2, counter1 ), CD_Crs( 2, counter2 ) /), & 
						ARG= (/ SlpMachLine( counter3 ), SlpMachLine( counter4 ) /) &
					)
			ENDDO

			! on the wall of nozzle
			counter0 = counter0 + 1
			counter1 = counter1 + 1
			counter2 = counter1 + ( Num_MachLines_thrt - itr_AreaLv1 + 1 )
			counter3 = counter3 + 1
			counter4 = counter3 + ( Num_MachLines_thrt - itr_AreaLv1 )
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
		CD_Div( 1:2, 1 ) = (/ CDX_Div_end, CDY_Div_end /)

		! STEP.02
		! on the wall of nozzle
		counter0 = Num_MachLines_thrt + 1
		counter1 = Num_MachLines_thrt * 2

		DO itr= Num_MachLines_thrt, 2*Num_MachLines_thrt-1, 1

			CD_Div( 1:2, itr ) = CalcCrossPoint( & 
				CDX= (/ CD_Div( 1, itr-1 ), CD_Crs( 1, counter0 ) /), & 
				CDY= (/ CD_Div( 1, itr-1 ), CD_Crs( 2, counter0 ) /), & 
				ARG= (/ VelcAng_Area( counter0 ), SlpMachLine( counter1 ) /) & 
			)

			counter0 = counter0 + (Num_DivPnts+1) - itr
			counter1 = counter1 + 2*(Num_DivPnts-itr)

		ENDDO



	! STEP.10
	! OPEN( UNIT= UnitNum, DEFAULTFILE= path_fldr_save, FILE= path_file_save, IOSTAT= val_stat, ACTION= 'WRITE', STATUS= 'REPLACE' )


	! STEP.11
	! CLOSE( UNIT= UnitNum, STATUS= 'KEEP', IOSTAT= val_stat )

	! STEP.12
	! deallocation
	DEALLOCATE(        CD_Crs, STAT= val_stat )
	DEALLOCATE(        CD_Div, STAT= val_stat )
	DEALLOCATE(   RmnnCnsvVal, STAT= val_stat )
	DEALLOCATE( SlpCharLine, STAT= val_stat )
	DEALLOCATE(  VelcAng_Area, STAT= val_stat )
	DEALLOCATE(  MachNum_Area, STAT= val_stat )
	DEALLOCATE(   PM_func_Area, STAT= val_stat )
	DEALLOCATE( SlpMachLine, STAT= val_stat )
	DEALLOCATE( SlpWallArea, STAT= val_stat )

	! STEP.END
	PRINT *, "ALL OVER"


END PROGRAM MAIN