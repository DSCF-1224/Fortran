!----------------------------------------------------------------------------------------------------------------------------------!
! [target]                                                                                                                         !
! https://qiita.com/SouTakenaka/items/a7dcfbde0a08063f4d41                                                                         !
!----------------------------------------------------------------------------------------------------------------------------------!
MODULE BZ_reaction

	! modules to use
	USE,     INTRINSIC :: ISO_FORTRAN_ENV ! gfortran 8.1.0
	USE, NON_INTRINSIC :: mt95
	USE, NON_INTRINSIC :: support_io

	! require all variables to be explicitly declared
	IMPLICIT NONE

	! constants for this <MODULE>
	INTEGER(KIND=INT32), PARAMETER, PUBLIC :: num_subst =   3_INT32
	INTEGER(KIND=INT32), PARAMETER, PUBLIC :: num_rcdx  = 100_INT32
	INTEGER(KIND=INT32), PARAMETER, PUBLIC :: num_rcdy  = 100_INT32

	REAL(KIND=REAL64), PARAMETER :: velocity(1:3) = (/ 1.0e+0_REAL64, 1.0e+0_REAL64, 1.0e+0_REAL64 /)

	! structures for this <MODULE>
	TYPE ConcentrationField
		REAL(KIND=REAL64), ALLOCATABLE :: crnt(:,:,:)
		REAL(KIND=REAL64), ALLOCATABLE :: buff(:,:,:)
	END TYPE ConcentrationField

	! <FUNCTION>s and <SUBROUTINE>s for this <MODULE>
	CONTAINS


	SUBROUTINE Allocation( field )

		! argument for this <SUBROUTINE>
		TYPE(ConcentrationField), INTENT(INOUT) :: field

		! local variable for this <SUBROUTINE>
		INTEGER(KIND=INT32) :: statval

		ALLOCATE( field%crnt( 1:num_rcdx, 1:num_rcdy, 1:num_subst ), STAT= statval, ERRMSG= buf_ErrMsg_io )
		CALL CheckStatAllocate( STAT= statval, ERRMSG= buf_ErrMsg_io )

		ALLOCATE( field%buff( 1:num_rcdx, 1:num_rcdy, 1:num_subst ), STAT= statval, ERRMSG= buf_ErrMsg_io )
		CALL CheckStatAllocate( STAT= statval, ERRMSG= buf_ErrMsg_io )

		RETURN

	END SUBROUTINE Allocation


	SUBROUTINE Deallocation( field )

		! argument for this <SUBROUTINE>
		TYPE(ConcentrationField), INTENT(INOUT) :: field

		! local variable for this <SUBROUTINE>
		INTEGER(KIND=INT32) :: statval

		DEALLOCATE( field%crnt, STAT= statval, ERRMSG= buf_ErrMsg_io )
		CALL CheckStatDeallocate( STAT= statval, ERRMSG= buf_ErrMsg_io )

		DEALLOCATE( field%buff, STAT= statval, ERRMSG= buf_ErrMsg_io )
		CALL CheckStatDeallocate( STAT= statval, ERRMSG= buf_ErrMsg_io )

		RETURN

	END SUBROUTINE Allocation


	SUBROUTINE Initialization( field )

		! argument for this <SUBROUTINE>
		TYPE(ConcentrationField), INTENT(INOUT) :: field

		CALL GENRAND_INIT( PUT= 1 )
		CALL GENRAND_REAL3( field%main(:,:,:) )
		RETURN

	END SUBROUTINE Initialization


	SUBROUTINE　UpdateConcentration( field )

		! argument for this <SUBROUTINE>
		TYPE(ConcentrationField), INTENT(INOUT) :: field

		! support variables for this <SUBROUTINE>
		INTEGER(KIND=INT32) :: itr_x, itr_y, itr_s

		! STEP.01
		! copy the cu
	END SUBROUTINE　UpdateConcentration




END MODULE BZ_reaction
