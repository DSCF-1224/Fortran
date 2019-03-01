MODULE math_constants

	! import modules to use
	USE nrtype

	! Require all variables to be explicitly declared
	IMPLICIT NONE

	REAL( KIND= SPR ), PARAMETER, PUBLIC :: MATHCNST_PI_SPR = ACOS( -1.0E+0_SPR )
	REAL( KIND= DPR ), PARAMETER, PUBLIC :: MATHCNST_PI_DPR = ACOS( -1.0E+0_DPR )
	REAL( KIND= QPR ), PARAMETER, PUBLIC :: MATHCNST_PI_QPR = ACOS( -1.0E+0_QPR )

	REAL( KIND= SPR ), PARAMETER, PRIVATE :: COEF_DEG2RAD_SPR = MATHCNST_PI_SPR / 1.80E+2_SPR
	REAL( KIND= DPR ), PARAMETER, PRIVATE :: COEF_DEG2RAD_DPR = MATHCNST_PI_DPR / 1.80E+2_DPR
	REAL( KIND= QPR ), PARAMETER, PRIVATE :: COEF_DEG2RAD_QPR = MATHCNST_PI_QPR / 1.80E+2_QPR

	REAL( KIND= SPR ), PARAMETER, PRIVATE :: COEF_RAD2DEG_SPR = 1.80E+2_SPR / MATHCNST_PI_SPR
	REAL( KIND= DPR ), PARAMETER, PRIVATE :: COEF_RAD2DEG_DPR = 1.80E+2_DPR / MATHCNST_PI_DPR
	REAL( KIND= QPR ), PARAMETER, PRIVATE :: COEF_RAD2DEG_QPR = 1.80E+2_QPR / MATHCNST_PI_QPR

	INTERFACE DEG2RAD
		MODULE PROCEDURE DEG2RAD_SPR
		MODULE PROCEDURE DEG2RAD_DPR
		MODULE PROCEDURE DEG2RAD_QPR
	END INTERFACE DEG2RAD

	INTERFACE RAD2DEG
		MODULE PROCEDURE RAD2DEG_SPR
		MODULE PROCEDURE RAD2DEG_DPR
		MODULE PROCEDURE RAD2DEG_QPR
	END INTERFACE RAD2DEG

	CONTAINS

	! convert the value of degree to one of radian [simple precision]
	PURE FUNCTION DEG2RAD_SPR ( deg ) RESULT( rad )

		! type of the argument of this <FUNCTION>
		REAL( KIND= SPR ), INTENT(IN) :: deg

		! type of the return value of this <FUNCTION>
		REAL( KIND= SPR ) :: rad

		rad = COEF_DEG2RAD_SPR * deg; RETURN

	END FUNCTION DEG2RAD_SPR

	! convert the value of degree to one of radian [double precision]
	PURE FUNCTION DEG2RAD_DPR ( deg ) RESULT( rad )

		! type of the argument of this <FUNCTION>
		REAL( KIND= DPR ), INTENT(IN) :: deg

		! type of the return value of this <FUNCTION>
		REAL( KIND= DPR ) :: rad

		rad = COEF_DEG2RAD_DPR * deg; RETURN

	END FUNCTION DEG2RAD_DPR

	! convert the value of degree to one of radian [quadruple precision]
	PURE FUNCTION DEG2RAD_QPR ( deg ) RESULT( rad )

		! type of the argument of this <FUNCTION>
		REAL( KIND= QPR ), INTENT(IN) :: deg

		! type of the return value of this <FUNCTION>
		REAL( KIND= QPR ) :: rad

		rad = COEF_DEG2RAD_QPR * deg; RETURN

	END FUNCTION DEG2RAD_QPR



	! convert the value of radian to one of degree [simple precision]
	PURE FUNCTION RAD2DEG_SPR ( rad ) RESULT( deg )

		! type of the argument of this <FUNCTION>
		REAL( KIND= SPR ), INTENT(IN) :: rad

		! type of the return value of this <FUNCTION>
		REAL( KIND= SPR ) :: deg

		deg = COEF_RAD2DEG_SPR * rad; RETURN

	END FUNCTION RAD2DEG_SPR

	! convert the value of radian to one of degree [double precision]
	PURE FUNCTION RAD2DEG_DPR ( rad )  RESULT( deg )

		! type of the argument of this function
		REAL( KIND= DPR ), INTENT(IN) :: rad

		! type of the return value of this <FUNCTION>
		REAL( KIND= DPR ) :: deg

		deg = COEF_RAD2DEG_DPR * rad; RETURN

	END FUNCTION RAD2DEG_DPR

	! convert the value of radian to one of degree [quadruple precision]
	PURE FUNCTION RAD2DEG_QPR ( rad )  RESULT( deg )

		! type of the argument of this function
		REAL( KIND= QPR ), INTENT(IN) :: rad

		! type of the return value of this <FUNCTION>
		REAL( KIND= QPR ) :: deg

		deg = COEF_RAD2DEG_QPR * rad; RETURN

	END FUNCTION RAD2DEG_QPR


END MODULE math_constants