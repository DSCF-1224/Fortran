! file created : 2018.10.29
! file updated : 2018.10.29

MODULE PrandtlMeyerFunc

	! import modules to use
	USE nrtype
	USE math_constants
	USE conditions
	USE support_Alloc
	USE support_IO

	! Require all variables to be explicitly declared
	IMPLICIT NONE

	! local constants in this `MODULE`
	REAL( KIND= DPR ), PARAMETER, PRIVATE :: COEF_SPHR_DPR = SQRT( ( SPHR_DPR+1.0e+0_DPR )/( SPHR_DPR-1.0e+0_DPR ) ) ! coefficient for Prandtl-Meyer Function
	REAL( KIND= QPR ), PARAMETER, PRIVATE :: COEF_SPHR_QPR = SQRT( ( SPHR_QPR+1.0e+0_QPR )/( SPHR_QPR-1.0e+0_QPR ) ) ! coefficient for Prandtl-Meyer Function

	INTEGER( KIND= I4B ), PARAMETER, PRIVATE :: upprlim_secantmethod = 100000

	INTERFACE PM_Func
		MODULE PROCEDURE PM_Func_DPR
		MODULE PROCEDURE PM_Func_QPR
	END INTERFACE PM_Func

	INTERFACE iPM_Func
		MODULE PROCEDURE iPM_Func_Secant_DPR
		MODULE PROCEDURE iPM_Func_Secant_QPR
		! MODULE PROCEDURE iPM_Func_NewtonRaphson_DPR
		! MODULE PROCEDURE iPM_Func_NewtonRaphson_QPR
	END INTERFACE iPM_Func

	INTERFACE SecantMethod
		MODULE PROCEDURE SecantMethod_DPR
		MODULE PROCEDURE SecantMethod_QPR
	END INTERFACE SecantMethod

	CONTAINS



	! Prandtl-Meyer Function
	! from Mach number to the angle [radian]
	PURE FUNCTION PM_Func_DPR( mach_num ) RESULT( arg_rad )

		! type of the argument of this <FUNCTION>
		REAL( KIND= DPR ), INTENT(IN) :: mach_num ! argument of the Prandtl-Meyer Function

		! type of the return value of this <FUNCTION>
		REAL( KIND= DPR ) :: arg_rad

		! local variables in this <FUNCTION>
		REAL( KIND= DPR ) :: buff_mach

		IF ( mach_num .LT. 1.0e+0_DPR ) THEN
			arg_rad = SQRT( -1.0e+0_DPR ); RETURN
		ELSE
			buff_mach = SQRT( mach_num * mach_num - 1.0e+0_DPR )
			arg_rad   = COEF_SPHR_DPR * ATAN( buff_mach / COEF_SPHR_DPR ) - ATAN( buff_mach )
			RETURN
		ENDIF

	END FUNCTION PM_Func_DPR

	PURE FUNCTION PM_Func_QPR( mach_num ) RESULT( arg_rad )

		! type of the argument of this <FUNCTION>
		REAL( KIND= QPR ), INTENT(IN) :: mach_num ! argument of the Prandtl-Meyer Function

		! type of the return value of this <FUNCTION>
		REAL( KIND= QPR ) :: arg_rad

		! local variables in this <FUNCTION>
		REAL( KIND= QPR ) :: buff_mach

		IF ( mach_num .LT. 1.0e+0_DPR ) THEN
			arg_rad = SQRT( -1.0e+0_QPR ); RETURN
		ELSE
			buff_mach = SQRT( mach_num * mach_num - 1.0e+0_QPR )
			arg_rad   = COEF_SPHR_QPR * ATAN( buff_mach / COEF_SPHR_QPR ) - ATAN( buff_mach )
			RETURN
		ENDIF

	END FUNCTION PM_Func_QPR



	! inverse Prandtl-Meyer Function
	! from the angle [radian] to Mach number
	! using secant method
	PURE FUNCTION iPM_Func_Secant_DPR( angle_rad, errr ) RESULT( mach_num )

		! type of the argument of this <FUNCTION>
		REAL( KIND= DPR ), INTENT(IN) :: angle_rad ! argument of the inverse Prandtl-Meyer Function
		REAL( KIND= DPR ), INTENT(IN) :: errr     ! allowable error of iterative method

		! type of the return value in this <FUNCTION>
		REAL( KIND= DPR ) :: mach_num

		IF ( angle_rad .LE. 0.0e+0_DPR ) THEN
			mach_num = SQRT( -1.0e+0_DPR ); RETURN
		ELSEIF ( angle_rad .EQ. 0.0e+0_DPR ) THEN
			mach_num = 1.0e+0_DPR; RETURN
		ELSE
			mach_num = SecantMethod_DPR( EQUATION= PM_Func_DPR, INIT1= 1.0e+0_DPR, INIT2= 1.5e+0_DPR, TARGET= angle_rad, ERRR= errr )
			RETURN
		ENDIF

	END FUNCTION iPM_Func_Secant_DPR

	PURE FUNCTION iPM_Func_Secant_QPR( angle_rad, errr ) RESULT( mach_num )

		! type of the argument of this <FUNCTION>
		REAL( KIND= QPR ), INTENT(IN) :: angle_rad ! argument of the inverse Prandtl-Meyer Function
		REAL( KIND= QPR ), INTENT(IN) :: errr     ! allowable error of iterative method

		! type of the return value in this <FUNCTION>
		REAL( KIND= QPR ) :: mach_num

		IF ( angle_rad .LE. 0.0e+0_DPR ) THEN
			mach_num = SQRT( -1.0e+0_QPR ); RETURN
		ELSEIF ( angle_rad .EQ. 0.0e+0_DPR ) THEN
			mach_num = 0.0e+0_QPR ; RETURN
		ELSE
			mach_num = SecantMethod_QPR( EQUATION= PM_Func_QPR, INIT1= 1.0e+0_QPR, INIT2= 1.5e+0_QPR, TARGET= angle_rad, ERRR= errr )
			RETURN
		ENDIF

	END FUNCTION iPM_Func_Secant_QPR



	! inverse Prandtl-Meyer Function
	! from the angle [radian] to Mach number
	! using Newton-Raphson method
	PURE FUNCTION iPM_Func_NewtonRaphson_DPR( angle_rad, errr ) RESULT( mach_num )

		! type of the argument of this <FUNCTION>
		REAL( KIND= DPR ), INTENT(IN) :: angle_rad ! argument of the inverse Prandtl-Meyer Function
		REAL( KIND= DPR ), INTENT(IN) :: errr     ! allowable error of iterative method

		! type of the return value in this <FUNCTION>
		REAL( KIND= DPR ) :: mach_num

		IF ( angle_rad .LE. 0.0e+0_DPR ) THEN
			mach_num = SQRT( -1.0e+0_DPR ); RETURN
		ELSEIF ( angle_rad .EQ. 0.0e+0_DPR ) THEN
			mach_num = 1.0e+0_DPR; RETURN
		ELSE
			mach_num = NewtonRaphsonForIPMF_DPR( INIT= 1.0e+2_DPR, TARGET= angle_rad, ERRR= errr )
			RETURN
		ENDIF

	END FUNCTION iPM_Func_NewtonRaphson_DPR

	PURE FUNCTION iPM_Func_NewtonRaphson_QPR( angle_rad, errr ) RESULT( mach_num )

		! type of the argument of this <FUNCTION>
		REAL( KIND= QPR ), INTENT(IN) :: angle_rad ! argument of the inverse Prandtl-Meyer Function
		REAL( KIND= QPR ), INTENT(IN) :: errr     ! allowable error of iterative method

		! type of the return value in this <FUNCTION>
		REAL( KIND= QPR ) :: mach_num

		IF ( angle_rad .LE. 0.0e+0_QPR ) THEN
			mach_num = SQRT( -1.0e+0_QPR ); RETURN
		ELSEIF ( angle_rad .EQ. 0.0e+0_QPR ) THEN
			mach_num = 1.0e+0_QPR; RETURN
		ELSE
			mach_num = NewtonRaphsonForIPMF_QPR( INIT= 1.0e+2_QPR, TARGET= angle_rad, ERRR= errr )
			RETURN
		ENDIF

	END FUNCTION iPM_Func_NewtonRaphson_QPR



	! Secant Method
	PURE FUNCTION SecantMethod_DPR( equation, init1, init2, target, errr ) RESULT( solution )

		! type of the arguments of this <FUNCTION>
		REAL( KIND= DPR ), INTENT(IN) :: init1, init2 ! initial condition
		REAL( KIND= DPR ), INTENT(IN) :: target       ! target for the solution
		REAL( KIND= DPR ), INTENT(IN) :: errr         ! allowable error of the secant method

		! type of the <FUNCTION> as the argument of this <FUNCTION>
		INTERFACE

			PURE FUNCTION equation( idvl )

				! import modules to use
				USE nrtype
				USE math_constants

				! Require all variables to be explicitly declared
				IMPLICIT NONE

				! type of the argument of this <FUNCTION>
				REAL( KIND= DPR ), INTENT(IN) :: idvl ! independent variable

				! type of the return value of this <FUNCTION>
				REAL( KIND= DPR ) :: equation

			END FUNCTION equation

		END INTERFACE

		! type of the return value of this <FUNCTION>
		REAL( KIND= DPR ) :: solution

		! type of the local variables in this <FUNCTION>
		REAL( KIND= DPR ), DIMENSION(-1:upprlim_secantmethod) :: solution_aprx
		REAL( KIND= DPR ), DIMENSION(-1:upprlim_secantmethod) :: solution_func
		REAL( KIND= DPR ), DIMENSION(-1:upprlim_secantmethod) :: solution_errr

		! type of the local variables for support in this <FUNCTION>
		INTEGER( KIND= I4B ) :: itr

		! STEP.01
		! set the initial conditions

			! STEP.01.01
			! set the initial condition of the argument
			solution_aprx(-1) = init1
			solution_aprx( 0) = init2

			! STEP.01.02
			! calculate the return value of the function and
			solution_func(-1:0:1) = (/ ( equation( solution_aprx( itr ) ), itr=-1,0,1 ) /)
			solution_errr(-1:0:1) = target -  solution_func(-1:0:1)

		! STEP.02
		! calculate the solution
		DO itr = 1, upprlim_secantmethod, 1

			! STEP.02.01
			! calculate the next approximate solution
			solution_aprx( itr ) = solution_aprx( itr-1 ) + ( solution_func( itr-1 ) - solution_func( itr-2 ) )/( solution_aprx( itr-1 ) - solution_aprx( itr-2 ) ) * solution_errr( itr-1 )
			solution_func( itr ) = equation( solution_aprx( itr ) )
			solution_errr( itr ) = target - solution_func( itr )

			! STEP.02.02
			! test the convergence
			IF( ABS( ( solution_aprx( itr ) - solution_aprx( itr-1 ) )/solution_aprx( itr ) ) .LT. errr ) THEN
				! solution = REAL( itr, KIND=DPR )
				solution = solution_aprx( itr )
				RETURN
			ELSE
				CONTINUE
			ENDIF
			
		ENDDO

		! STEP.03
		! if the convergence condition was not satisfied
		IF ( itr .EQ. upprlim_secantmethod ) THEN
			solution = SQRT( -1.0e+0_DPR )
			RETURN
		ENDIF


	END FUNCTION SecantMethod_DPR

	PURE FUNCTION SecantMethod_QPR( equation, init1, init2, target, errr ) RESULT( solution )

		! type of the arguments of this <FUNCTION>
		REAL( KIND= QPR ), INTENT(IN) :: init1, init2 ! initial condition
		REAL( KIND= QPR ), INTENT(IN) :: target       ! target for the solution
		REAL( KIND= QPR ), INTENT(IN) :: errr         ! allowable error of the secant method

		! type of the <FUNCTION> as the argument of this <FUNCTION>
		INTERFACE

			PURE FUNCTION equation( idvl )

				! import modules to use
				USE nrtype
				USE math_constants

				! Require all variables to be explicitly declared
				IMPLICIT NONE

				! type of the argument of this <FUNCTION>
				REAL( KIND= QPR ), INTENT(IN) :: idvl ! independent variable

				! type of the return value of this <FUNCTION>
				REAL( KIND= QPR ) :: equation

			END FUNCTION equation

		END INTERFACE

		! type of the return value of this <FUNCTION>
		REAL( KIND= QPR ) :: solution

		! type of the local variables in this <FUNCTION>
		REAL( KIND= QPR ), DIMENSION(-1:upprlim_secantmethod) :: solution_aprx
		REAL( KIND= QPR ), DIMENSION(-1:upprlim_secantmethod) :: solution_func
		REAL( KIND= QPR ), DIMENSION(-1:upprlim_secantmethod) :: solution_errr

		! type of the local variables for support in this <FUNCTION>
		INTEGER( KIND= I4B ) :: itr

		! STEP.01
		! set the initial conditions

			! STEP.01.01
			! set the initial condition of the argument
			solution_aprx(-1) = init1
			solution_aprx( 0) = init2

			! STEP.01.02
			! calculate the return value of the function and
			solution_func(-1:0:1) = (/ ( equation( solution_aprx( itr ) ), itr=-1,0,1 ) /)
			solution_errr(-1:0:1) = target -  solution_func(-1:0:1)

		! STEP.02
		! calculate the solution
		DO itr = 1, upprlim_secantmethod, 1

			! STEP.02.01
			! calculate the next approximate solution
			solution_aprx( itr ) = solution_aprx( itr-1 ) + ( solution_func( itr-1 ) - solution_func( itr-2 ) )/( solution_aprx( itr-1 ) - solution_aprx( itr-2 ) ) * solution_errr( itr-1 )
			solution_func( itr ) = equation( solution_aprx( itr ) )
			solution_errr( itr ) = target - solution_func( itr )

			! STEP.02.02
			! test the convergence
			IF( ABS( ( solution_aprx( itr ) - solution_aprx( itr-1 ) )/solution_aprx( itr ) ) .LT. errr ) THEN
				! solution = REAL( itr, KIND=QPR )
				solution = solution_aprx( itr )
				RETURN
			ELSE
				CONTINUE
			ENDIF
			
		ENDDO

		! STEP.03
		! if the convergence condition was not satisfied
		IF ( itr .EQ. upprlim_secantmethod ) THEN
			solution = SQRT( -1.0e+0_QPR )
			RETURN
		ENDIF


	END FUNCTION SecantMethod_QPR



	! Newton-Raphson Method Only for inverse Prandtl-Meyer Function
	PURE FUNCTION NewtonRaphsonForIPMF_DPR( init, target, errr ) RESULT( solution )

		! type of the arguments of this <FUNCTION>
		REAL( KIND= DPR ), INTENT(IN) :: init   ! initial condition
		REAL( KIND= DPR ), INTENT(IN) :: target ! target for the solution
		REAL( KIND= DPR ), INTENT(IN) :: errr   ! allowable error of the secant method

		! type of the return value of this <FUNCTION>
		REAL( KIND= DPR ) :: solution

		! type of the local variables in this <FUNCTION>
		REAL( KIND= DPR ), DIMENSION(0:upprlim_secantmethod) :: mach_num
		REAL( KIND= DPR ), DIMENSION(0:upprlim_secantmethod) :: pmf_rad

		REAL( KIND= DPR ) :: buff_mach

		! type of the local variables for support in this <FUNCTION>
		INTEGER( KIND= I4B ) :: itr

		! STEP.01
		! set the initial conditions
		mach_num(0) = init
		pmf_rad (0) = PM_Func_DPR( init )

		! STEP.02
		! calculate the solution
		DO itr = 1, upprlim_secantmethod, 1

			! STEP.02.01
			! calculate the next approximate solution
			buff_mach       = SQRT( mach_num( itr-1 )*mach_num( itr-1 ) - 1.0e+0_DPR )
			mach_num( itr ) = mach_num( itr-1 ) + ( target - pmf_rad( itr-1 ) ) * ( mach_num( itr-1 )*( COEF_SPHR_DPR*COEF_SPHR_DPR + buff_mach*buff_mach ) ) / ( ( COEF_SPHR_DPR*COEF_SPHR_DPR-1.0e+0_DPR ) * buff_mach )

			! STEP.02.02
			! test the convergence
			IF( ABS( ( mach_num( itr ) - mach_num( itr-1 ) )/mach_num( itr ) ) .LT. errr ) THEN
				! solution = REAL( itr, KIND=DPR )
				solution = mach_num( itr )
				RETURN
			ELSE
				CONTINUE
			ENDIF
			
		ENDDO

		! STEP.03
		! if the convergence condition was not satisfied
		IF ( itr .EQ. upprlim_secantmethod ) THEN
			solution = SQRT( -1.0e+0_DPR )
			RETURN
		ENDIF

	END FUNCTION NewtonRaphsonForIPMF_DPR

	PURE FUNCTION NewtonRaphsonForIPMF_QPR( init, target, errr ) RESULT( solution )

		! type of the arguments of this <FUNCTION>
		REAL( KIND= QPR ), INTENT(IN) :: init   ! initial condition
		REAL( KIND= QPR ), INTENT(IN) :: target ! target for the solution
		REAL( KIND= QPR ), INTENT(IN) :: errr   ! allowable error of the secant method

		! type of the return value of this <FUNCTION>
		REAL( KIND= QPR ) :: solution

		! type of the local variables in this <FUNCTION>
		REAL( KIND= QPR ), DIMENSION(0:upprlim_secantmethod) :: mach_num
		REAL( KIND= QPR ), DIMENSION(0:upprlim_secantmethod) :: pmf_rad

		REAL( KIND= QPR ) :: buff_mach

		! type of the local variables for support in this <FUNCTION>
		INTEGER( KIND= I4B ) :: itr

		! STEP.01
		! set the initial conditions
		mach_num(0) = init
		pmf_rad (0) = PM_Func_QPR( init )

		! STEP.02
		! calculate the solution
		DO itr = 1, upprlim_secantmethod, 1

			! STEP.02.01
			! calculate the next approximate solution
			buff_mach       = SQRT( mach_num( itr-1 )*mach_num( itr-1 ) - 1.0e+0_QPR )
			mach_num( itr ) = mach_num( itr-1 ) - ( target - pmf_rad( itr-1 ) ) * ( mach_num( itr-1 )*( COEF_SPHR_QPR*COEF_SPHR_QPR + buff_mach*buff_mach ) ) / ( ( COEF_SPHR_QPR*COEF_SPHR_QPR-1.0e+0_QPR ) * buff_mach )

			! STEP.02.02
			! test the convergence
			IF( ABS( ( mach_num( itr ) - mach_num( itr-1 ) )/mach_num( itr ) ) .LT. errr ) THEN
				! solution = REAL( itr, KIND=QPR )
				solution = mach_num( itr )
				RETURN
			ELSE
				CONTINUE
			ENDIF
			
		ENDDO

		! STEP.03
		! if the convergence condition was not satisfied
		IF ( itr .EQ. upprlim_secantmethod ) THEN
			solution = SQRT( -1.0e+0_QPR )
			RETURN
		ENDIF



	END FUNCTION NewtonRaphsonForIPMF_QPR



	! [TEST]
	! Prandtl-Meyer Function and inverse Prandtl-Meyer FUNCTION
	SUBROUTINE test_PMF_DPR( unitnum, num_points, errr, path_fldr, name_file )

		! type of the argument of this <SUBROUTINE>
		INTEGER( KIND= I4B ), INTENT(IN) :: unitnum    ! unit number for output
		INTEGER( KIND= I4B ), INTENT(IN) :: num_points ! the number of test points

		REAL( KIND= DPR ), INTENT(IN) :: errr ! allowable error

		CHARACTER( LEN=* ), INTENT(IN) :: path_fldr
		CHARACTER( LEN=* ), INTENT(IN) :: name_file

		! type of the local variables for this <SUBROUTINE>

		REAL( KIND= DPR ) :: stepsize_mach_num = 1.0e-2_DPR

		REAL( KIND= DPR ), DIMENSION(:), ALLOCATABLE :: mach_num
		REAL( KIND= DPR ), DIMENSION(:), ALLOCATABLE :: pmf_rad
		REAL( KIND= DPR ), DIMENSION(:), ALLOCATABLE :: pmf_deg
		REAL( KIND= DPR ), DIMENSION(:), ALLOCATABLE :: ipmf

		! type of the support variables for this <SUBROUTINE>
		INTEGER( KIND= I4B ) :: itr
		INTEGER( KIND= I4B ) :: lbound_mach_num, ubound_mach_num
		INTEGER( KIND= I4B ) :: statval, errmsgval

		CHARACTER( LEN= 128 ) :: buff_errmsg

		! STEP.01
		! allocation of arrays to store the result of calculation
		ALLOCATE( mach_num(1:num_points), STAT=statval, ERRMSG= buff_errmsg ); CALL CheckStatAlloc( VAL= statval, NAME= "mach_num", MSG= buff_errmsg )
		ALLOCATE(  pmf_rad(1:num_points), STAT=statval, ERRMSG= buff_errmsg ); CALL CheckStatAlloc( VAL= statval, NAME= "pmf_rad",  MSG= buff_errmsg )
		ALLOCATE(  pmf_deg(1:num_points), STAT=statval, ERRMSG= buff_errmsg ); CALL CheckStatAlloc( VAL= statval, NAME= "pmf_deg",  MSG= buff_errmsg )
		ALLOCATE(     ipmf(1:num_points), STAT=statval, ERRMSG= buff_errmsg ); CALL CheckStatAlloc( VAL= statval, NAME= "ipmf",     MSG= buff_errmsg )

		! STEP.02
		! open the file to save the result
		OPEN( & 
			UNIT        = unitnum, & 
			DEFAULTFILE = TRIM( path_fldr ), & 
			FILE        = TRIM( name_file ), & 
			IOSTAT      = statval, & 
			ACTION      = 'WRITE', & 
			STATUS      = 'REPLACE' & 
		)
		CALL CheckIostatOpen( VAL= statval, PATH= TRIM( path_fldr ) // '\' // TRIM( name_file ) )

		! STEP.03
		! record the result of calculation of Prandtl-Meyer Function

			! STEP.03.01
			! store the size of array
			lbound_mach_num= LBOUND( mach_num, 1 )
			ubound_mach_num= UBOUND( mach_num, 1 )

			! STEP.03.02
			! set the components of array of Mach number
			mach_num(:) = (/ ( stepsize_mach_num * REAL( itr, KIND= DPR ), itr= lbound_mach_num,ubound_mach_num,1 ) /)
			mach_num(:) = mach_num(:) + 1.0e+0_QPR

			! STEP.03.03
			! calculate the Prandtl-Meyer function
			pmf_rad(:) = (/ ( PM_Func( mach_num( itr ) ), itr= lbound_mach_num,ubound_mach_num,1 ) /)
			pmf_deg(:) = (/ ( RAD2DEG(  pmf_rad( itr ) ), itr= lbound_mach_num,ubound_mach_num,1 ) /)

			! STEP.03.04
			! calculate the inverse Prandtl-Meyer function
			ipmf(:) = (/ ( iPM_Func( pmf_rad( itr ), errr ), itr= lbound_mach_num,ubound_mach_num,1 ) /)

		! STEP.04
		! output the result of calculation

			! STEP.04.01
			! output the index
			WRITE( UNIT= unitnum, FMT= '(A)', ADVANCE='NO',  IOSTAT= statval ) 'Mach number [-],'
			WRITE( UNIT= unitnum, FMT= '(A)', ADVANCE='NO',  IOSTAT= statval ) 'PMF [deg],'
			WRITE( UNIT= unitnum, FMT= '(A)', ADVANCE='NO',  IOSTAT= statval ) 'inverse PMF [-],'
			WRITE( UNIT= unitnum, FMT= '(A)', ADVANCE='NO',  IOSTAT= statval ) 'PMF [deg] from inverse,'
			WRITE( UNIT= unitnum, FMT= '(A)', ADVANCE='NO',  IOSTAT= statval ) 'error PMF,'
			WRITE( UNIT= unitnum, FMT= '(A)', ADVANCE='YES', IOSTAT= statval ) 'error iPMF'

			! STEP.04.02
			! output the result of calculation
			DO itr = lbound_mach_num,ubound_mach_num,1
				WRITE( UNIT= unitnum, FMT='(ES22.14,5(",",ES22.14))', ADVANCE='YES', IOSTAT= statval ) & 
					mach_num( itr ), & 
					pmf_deg( itr ),  & 
					ipmf( itr ),  & 
					RAD2DEG( PM_Func( ipmf( itr ) ) ), & 
					ABS( RAD2DEG( ( pmf_rad( itr ) - PM_Func( ipmf( itr ) ) ) / pmf_rad( itr ) ) ), &
					ABS( ( mach_num( itr ) - ipmf( itr ) ) / mach_num( itr ) )
			ENDDO

		! STEP.05
		! close the file outputted the result
		CLOSE( UNIT= unitnum, STATUS='KEEP', IOSTAT= statval )

		! STEP.06
		! deallocation
		DEALLOCATE( mach_num, STAT=statval, ERRMSG= buff_errmsg ); CALL CheckStatDealloc( VAL= statval, NAME= "mach_num",MSG= buff_errmsg )
		DEALLOCATE(  pmf_rad, STAT=statval, ERRMSG= buff_errmsg ); CALL CheckStatDealloc( VAL= statval, NAME= "pmf_rad", MSG= buff_errmsg )
		DEALLOCATE(  pmf_deg, STAT=statval, ERRMSG= buff_errmsg ); CALL CheckStatDealloc( VAL= statval, NAME= "pmf_deg", MSG= buff_errmsg )
		DEALLOCATE(     ipmf, STAT=statval, ERRMSG= buff_errmsg ); CALL CheckStatDealloc( VAL= statval, NAME= "ipmf",    MSG= buff_errmsg )

		! STEP.END
		RETURN

	END SUBROUTINE test_PMF_DPR

	SUBROUTINE test_PMF_QPR( unitnum, num_points, errr, path_fldr, name_file )

		! type of the argument of this <SUBROUTINE>
		INTEGER( KIND= I4B ), INTENT(IN) :: unitnum    ! unit number for output
		INTEGER( KIND= I4B ), INTENT(IN) :: num_points ! the number of test points

		REAL( KIND= QPR ), INTENT(IN) :: errr ! allowable error

		CHARACTER( LEN=* ), INTENT(IN) :: path_fldr
		CHARACTER( LEN=* ), INTENT(IN) :: name_file

		! type of the local variables for this <SUBROUTINE>

		REAL( KIND= QPR ) :: stepsize_mach_num = 1.0e-2_QPR

		REAL( KIND= QPR ), DIMENSION(:), ALLOCATABLE :: mach_num
		REAL( KIND= QPR ), DIMENSION(:), ALLOCATABLE :: pmf_rad
		REAL( KIND= QPR ), DIMENSION(:), ALLOCATABLE :: pmf_deg
		REAL( KIND= QPR ), DIMENSION(:), ALLOCATABLE :: ipmf

		! type of the support variables for this <SUBROUTINE>
		INTEGER( KIND= I4B ) :: itr
		INTEGER( KIND= I4B ) :: lbound_mach_num, ubound_mach_num
		INTEGER( KIND= I4B ) :: statval, errmsgval

		CHARACTER( LEN= 128 ) :: buff_errmsg

		! STEP.01
		! allocation of arrays to store the result of calculation
		ALLOCATE( mach_num(1:num_points), STAT=statval, ERRMSG= buff_errmsg ); CALL CheckStatAlloc( VAL= statval, NAME= "mach_num", MSG= buff_errmsg )
		ALLOCATE(  pmf_rad(1:num_points), STAT=statval, ERRMSG= buff_errmsg ); CALL CheckStatAlloc( VAL= statval, NAME= "pmf_rad",  MSG= buff_errmsg )
		ALLOCATE(  pmf_deg(1:num_points), STAT=statval, ERRMSG= buff_errmsg ); CALL CheckStatAlloc( VAL= statval, NAME= "pmf_deg",  MSG= buff_errmsg )
		ALLOCATE(     ipmf(1:num_points), STAT=statval, ERRMSG= buff_errmsg ); CALL CheckStatAlloc( VAL= statval, NAME= "ipmf",     MSG= buff_errmsg )

		! STEP.02
		! open the file to save the result
		OPEN( & 
			UNIT        = unitnum, & 
			DEFAULTFILE = TRIM( path_fldr ), & 
			FILE        = TRIM( name_file ), & 
			IOSTAT      = statval, & 
			ACTION      = 'WRITE', & 
			STATUS      = 'REPLACE' & 
		)
		CALL CheckIostatOpen( VAL= statval, PATH= TRIM( path_fldr ) // '\' // TRIM( name_file ) )

		! STEP.03
		! record the result of calculation of Prandtl-Meyer Function

			! STEP.03.01
			! store the size of array
			lbound_mach_num= LBOUND( mach_num, 1 )
			ubound_mach_num= UBOUND( mach_num, 1 )

			! STEP.03.02
			! set the components of array of Mach number
			mach_num(:) = (/ ( stepsize_mach_num * REAL( itr, KIND= QPR ), itr= lbound_mach_num,ubound_mach_num,1 ) /)
			mach_num(:) = mach_num(:) + 1.0e+0_QPR

			! STEP.03.03
			! calculate the Prandtl-Meyer function
			pmf_rad(:) = (/ ( PM_Func( mach_num( itr ) ), itr= lbound_mach_num,ubound_mach_num,1 ) /)
			pmf_deg(:) = (/ ( RAD2DEG(  pmf_rad( itr ) ), itr= lbound_mach_num,ubound_mach_num,1 ) /)

			! STEP.03.04
			! calculate the inverse Prandtl-Meyer function
			ipmf(:) = (/ ( iPM_Func( pmf_rad( itr ), errr ), itr= lbound_mach_num,ubound_mach_num,1 ) /)

		! STEP.04
		! output the result of calculation

			! STEP.04.01
			! output the index
			WRITE( UNIT= unitnum, FMT= '(A)', ADVANCE='NO',  IOSTAT= statval ) 'Mach number [-],'
			WRITE( UNIT= unitnum, FMT= '(A)', ADVANCE='NO',  IOSTAT= statval ) 'PMF [deg],'
			WRITE( UNIT= unitnum, FMT= '(A)', ADVANCE='NO',  IOSTAT= statval ) 'inverse PMF [-],'
			WRITE( UNIT= unitnum, FMT= '(A)', ADVANCE='NO',  IOSTAT= statval ) 'PMF [deg] from inverse,'
			WRITE( UNIT= unitnum, FMT= '(A)', ADVANCE='NO',  IOSTAT= statval ) 'error PMF,'
			WRITE( UNIT= unitnum, FMT= '(A)', ADVANCE='YES', IOSTAT= statval ) 'error iPMF'

			! STEP.04.02
			! output the result of calculation
			DO itr = lbound_mach_num,ubound_mach_num,1
				WRITE( UNIT= unitnum, FMT='(ES22.14,5(",",ES22.14))', ADVANCE='YES', IOSTAT= statval ) & 
					mach_num( itr ), & 
					pmf_deg( itr ),  & 
					ipmf( itr ),  & 
					RAD2DEG( PM_Func( ipmf( itr ) ) ), & 
					ABS( RAD2DEG( ( pmf_rad( itr ) - PM_Func( ipmf( itr ) ) ) / pmf_rad( itr ) ) ), &
					ABS( ( mach_num( itr ) - ipmf( itr ) ) / mach_num( itr ) )
			ENDDO

		! STEP.05
		! close the file outputted the result
		CLOSE( UNIT= unitnum, STATUS='KEEP', IOSTAT= statval )

		! STEP.06
		! deallocation
		DEALLOCATE( mach_num, STAT=statval, ERRMSG= buff_errmsg ); CALL CheckStatDealloc( VAL= statval, NAME= "mach_num",MSG= buff_errmsg )
		DEALLOCATE(  pmf_rad, STAT=statval, ERRMSG= buff_errmsg ); CALL CheckStatDealloc( VAL= statval, NAME= "pmf_rad", MSG= buff_errmsg )
		DEALLOCATE(  pmf_deg, STAT=statval, ERRMSG= buff_errmsg ); CALL CheckStatDealloc( VAL= statval, NAME= "pmf_deg", MSG= buff_errmsg )
		DEALLOCATE(     ipmf, STAT=statval, ERRMSG= buff_errmsg ); CALL CheckStatDealloc( VAL= statval, NAME= "ipmf",    MSG= buff_errmsg )

		! STEP.END
		RETURN

	END SUBROUTINE test_PMF_QPR

	SUBROUTINE test_PMF_main

		CHARACTER( LEN= 68 ) :: path_fldr_tosave     = "D:\Aero\Documents\Visual Studio 2010\Projects\Multi_Project_Fortran"
		CHARACTER( LEN= 23 ) :: name_file_tosave_DPR = "test20181025_01_DPR.csv"
		CHARACTER( LEN= 23 ) :: name_file_tosave_QPR = "test20181025_01_QPR.csv"

		INTEGER( KIND= I4B ) :: num_points = 900

		CALL test_PMF_DPR( 10, num_points, 1.0e-14_DPR, path_fldr_tosave, name_file_tosave_DPR )
		CALL test_PMF_QPR( 11, num_points, 1.0e-14_QPR, path_fldr_tosave, name_file_tosave_QPR )

	END SUBROUTINE test_PMF_main

END MODULE PrandtlMeyerFunc