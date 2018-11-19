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

	FUNCTION MachAngleRad( mach )

		! argument for this <FUNCTION>
		DOUBLE PRECISION, INTENT(IN) :: mach ! mach number

		! return value of this <FUNCTION>
		DOUBLE PRECISION :: MachAngleRad

		MachAngleRad = ASIN( 1.0D+0/mach )
		RETURN

	END FUNCTION MachAngleRad


END MODULE SuperSonicNozzle



PROGRAM MAIN

	! imported <MODULE>s
	USE PrandtlMeyerNrml
	USE PrandtlMeyerInvs
	USE SuperSonicNozzle

	! Require all variables to be explicitly declared
	IMPLICIT NONE



	! constants in this <PROGRAM>
	DOUBLE PRECISION, PARAMETER :: CDX_start      = 0.0D+0 ! 
	DOUBLE PRECISION, PARAMETER :: CDY_start      = 5.0D-1 !
	DOUBLE PRECISION, PARAMETER :: CDX_end        = 0.0D+0 ! 
	DOUBLE PRECISION, PARAMETER :: CDY_end        = 3.0D-1 !
	DOUBLE PRECISION, PARAMETER :: sphr           = 1.4D+0 ! specific heat ratio [-]
	DOUBLE PRECISION, PARAMETER :: MachNum_thrt   = 1.0D+0 ! Mach number at the throat of the Laval nozzle
	DOUBLE PRECISION, PARAMETER :: MachNum_otlt   = 2.0D+0 ! Mach number at the outlet of the Laval nozzle
	DOUBLE PRECISION, PARAMETER :: SlopeWall_thrt = 0.0D+0 ! slope of the nozzle wall at the throat of the Laval nozzle [rad]
	DOUBLE PRECISION, PARAMETER :: SlopeWall_otlt = 0.0D+0 ! slope of the nozzle wall at the outlet of the Laval nozzle [rad]

	INTEGER( KIND= 4 ), PARAMETER :: Num_MachLines_thrt = 10 ! the number of the Mach Lines at the throat



	! local variables in this <PROGRAM>
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: CD_CrsPnt      ! coordinate of the cross points
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: RiemannCnsvVal ! Riemann's conservation value
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: SlopeCharLine  ! slope of the charastalic line

	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: VelcAngl_Area  ! angle the the velocity vectors at the each area defined by Mach lines
	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: MachAngl_Area  ! Mach angle at the each area defined by Mach lines [rad]
	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: MachNum_Area   ! Mach number at the each area defined by Mach lines
	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: PMfunc_Area    ! Prandtl-Meyer function at the each area defined by Mach lines
	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: SlopeMachLine  ! slope of the Mach line [rad]
	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: SlopeWall_Area ! slope of the nozzle wall [rad]

	INTEGER( KIND= 4 ), PARAMETER :: Num_CrsPnts ! the number of total cross points
	INTEGER( KIND= 4 ), PARAMETER :: Num_Areas   ! the number of the areas defined by Mach lines



	! support variables for this <PROGRAM>
	INTEGER( KIND= 4 ) :: val_stat

	! STEP.01
	! allocation
	Num_Areas   = ( Num_MachLines_thrt+1 )*( Num_MachLines_thrt*2 ) / 2
	Num_CrsPnts = ( Num_MachLines_thrt+1 )*  Num_MachLines_thrt     / 2

	ALLOCATE(      CD_CrsPnt(1:2,1:Num_CrsPnts), STAT= val_stat )
	ALLOCATE( RiemannCnsvVal(1:2,1:Num_Areas),   STAT= val_stat )
	ALLOCATE(  SlopeCharLine(1:2,1:Num_Areas),   STAT= val_stat )

	ALLOCATE(      VelcAngl_Area(1:Num_Areas),  STAT= val_stat )
	ALLOCATE(       MachNum_Area(1:Num_Areas),  STAT= val_stat )
	ALLOCATE(        PMfunc_Area(1:Num_Areas),  STAT= val_stat )
	ALLOCATE( SlopeWall_rad_Area(1:Num_Areas),  STAT= val_stat )

	! STEP.02
	! calculation at the throat section of the nozzle
	MachNum_Area( 1 )  = MachNum_thrt
	VelcAngl_Area( 1 ) = SlopeWall_thrt

	PMfunc_Area( 1 )   = PMfuncRad( MACH= MachNum_Area( 1 ), SPHR= sphr )
	MachAngl_Area( 1 ) = MachAngleRad( MachNum_Area( 1 ) )

	RiemannCnsvVal( 1, 1 ) = PMfunc_Area( 1 ) + MachAngl_Area( 1 ) ! left 
	RiemannCnsvVal( 2, 1 ) = PMfunc_Area( 1 ) - MachAngl_Area( 1 ) ! right

	SlopeCharLine( 1, 1 ) = VelcAngl_Area( 1 ) - MachAngl_Area( 1 ) ! left 
	SlopeCharLine( 2, 1 ) = VelcAngl_Area( 1 ) + MachAngl_Area( 1 ) ! right

	! STEP.03
	! calculation at the outlet of the nozzle
	MachNum_Area( Num_Areas )  = MachNum_otlt
	VelcAngl_Area( Num_Areas ) = SlopeWall_otlt

	PMfunc_Area( Num_Areas )   = PMfuncRad( MACH= MachNum_Area( Num_Areas ), SPHR= sphr )
	MachAngl_Area( Num_Areas ) = MachAngleRad( MachNum_Area( Num_Areas ) )

	RiemannCnsvVal( 1, Num_Areas ) = PMfunc_Area( Num_Areas ) + MachAngl_Area( Num_Areas ) ! left 
	RiemannCnsvVal( 2, Num_Areas ) = PMfunc_Area( Num_Areas ) - MachAngl_Area( Num_Areas ) ! right

	SlopeCharLine( 1, Num_Areas ) = VelcAngl_Area( Num_Areas ) - MachAngl_Area( Num_Areas ) ! left 
	SlopeCharLine( 2, Num_Areas ) = VelcAngl_Area( Num_Areas ) + MachAngl_Area( Num_Areas ) ! right



	! STEP.03
	OPEN( UNIT= UnitNum, DEFAULTFILE= path_fldr_save, FILE= path_file_save, IOSTAT= val_stat, ACTION= 'WRITE', STATUS= 'REPLACE' )


	! STEP.03
	CLOSE( UNIT= UnitNum, STATUS= 'KEEP', IOSTAT= val_stat )


END PROGRAM MAIN