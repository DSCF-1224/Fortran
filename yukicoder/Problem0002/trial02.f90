! yukicoder Problem 0002
! 
! code created : 2018.11.03
! code updated : 2018.11.03
!
! compiler : Fortran (gFortran 4.8.5)
!
! [reference]
! https://kimiyuki.net/writeup/algo/yukicoder/2/
! https://densanken.com/wiki/index.php?Grundy%BF%F4
! https://densanken.com/wiki/index.php?Nim

! http://numerical.recipes/pubdom/nrtype.f90.txt
MODULE nrtype
    
	! Require all variables to be explicitly declared
	IMPLICIT NONE

	INTEGER, PARAMETER :: I4B = SELECTED_INT_KIND(9)
	INTEGER, PARAMETER :: I2B = SELECTED_INT_KIND(4)
	INTEGER, PARAMETER :: I1B = SELECTED_INT_KIND(2)
	INTEGER, PARAMETER :: SPR = KIND(1.0)
	INTEGER, PARAMETER :: DPR = KIND(1.0D0)
	INTEGER, PARAMETER :: SPC = KIND((1.0,1.0))
	INTEGER, PARAMETER :: DPC = KIND((1.0D0,1.0D0))
	INTEGER, PARAMETER :: LGT = KIND(.TRUE.)

END MODULE nrtype



MODULE Exception

	! import modules to use
	USE nrtype

	! Require all variables to be explicitly declared
	IMPLICIT NONE

	CONTAINS

	! Exception for the `stat` of <allocate> statement
	SUBROUTINE exception_allocalte( val, name )

		! declaration of the arguments
		INTEGER(KIND= I4B), INTENT(IN) :: val  ! return value of the <STAT>
		CHARACTER(LEN= *),  INTENT(IN) :: name ! name of the target array

		IF ( val .GT. 0 ) THEN
			PRINT "(A)"  , "Error allocating `" // name // "`"
			PRINT "(A,I3)", "<STAT> is", val
			CALL stop_simple
		ELSE
			RETURN
		ENDIF

	END SUBROUTINE exception_allocalte

	! Exception for the `stat` of <deallocate> statement
	SUBROUTINE exception_deallocalte( val, name )

		! declaration of the arguments
		INTEGER(KIND= I4B), INTENT(IN) :: val  ! return value of the <STAT>
		CHARACTER(LEN= *),  INTENT(IN) :: name ! name of the target array

		IF ( val .GT. 0 ) THEN
			PRINT "(A)"  , "Error deallocating `" // name // "`"
			PRINT "(A,I3)", "<STAT> is", val
			CALL stop_simple
		ELSE
			RETURN
		ENDIF

	END SUBROUTINE exception_deallocalte

	! Exception for the `iostat` of <read> statement
	SUBROUTINE exception_read( val, name )

		! declaration of the arguments
		INTEGER(KIND= I4B), INTENT(IN) :: val  ! return value of the <IOSTAT>
		CHARACTER(LEN= *),  INTENT(IN) :: name ! name of the target variable

		IF ( val .GT. 0 ) THEN
			PRINT "(A)"  , "Error reading `" // name // "`"
			PRINT "(A,I3)", "<STAT> is", val
			CALL stop_simple
		ELSE
			RETURN
		ENDIF

	END SUBROUTINE exception_read

	! <STOP> statement with fixed and simple comment
	SUBROUTINE stop_simple
		STOP "<STOP> statement have been activated!"
	END SUBROUTINE stop_simple

END MODULE Exception



MODULE PrimeNumber

	! <MODULE> to use
	USE nrtype

	! Require all variables to be explicitly declared
	IMPLICIT NONE

	CONTAINS

	FUNCTION GetMaxPrimeFactor( target ) RESULT( mpf )

		! type of argument of this <FUNCTION>
		INTEGER(KIND= I4B), INTENT(IN) :: target

		! type of return value of this <FUNCTION>
		INTEGER(KIND= I4B) :: mpf ! maximum prime factor

		! type of local variable
		INTEGER(KIND= I4B) :: itr

		! STEP.01
		! initialize a iterator and return value of this function
		mpf = target
		itr = 2

		! STEP.02
		! search the maximum prime factor
		DO WHILE ( itr * itr .LE. mpf )
			DO WHILE ( MOD( mpf, itr ) .EQ. 0 )
				mpf = mpf / itr
			ENDDO
			itr = itr + 1
		ENDDO

		! STEP.END
		RETURN

	END FUNCTION GetMaxPrimeFactor



	FUNCTION GetPrimePowers( target, mpf ) RESULT( list )

		! type of argument of this <FUNCTION>
		INTEGER(KIND= I4B), INTENT(IN) :: target
		INTEGER(KIND= I4B), INTENT(IN) :: mpf    ! maximum prime factor of `target`

		! type of return value of this <FUNCTION>
		INTEGER(KIND= I4B), DIMENSION(2:mpf) :: list ! list of power of prme factors

		! type of local variable
		INTEGER(KIND= I4B) :: buf
		INTEGER(KIND= I4B) :: itr

		! STEP.01
		! initialize a iterator, buffer and return value of this function
		buf     = target
		itr     = 2
		list(:) = 0

		! STEP.02
		! search the maximum prime factor
		DO WHILE ( itr * itr .LE. buf )
			DO WHILE ( MOD( buf, itr ) .EQ. 0 )
				buf       = buf / itr
				list(itr) = list(itr) + 1
			ENDDO
			itr = itr + 1
		ENDDO

		! STEP.END
		RETURN

	END FUNCTION GetPrimePowers

END MODULE PrimeNumber



PROGRAM main

	! <MODULE> to use
	USE nrtype
	USE Exception
	USE PrimeNumber

	! Require all variables to be explicitly declared
	IMPLICIT NONE

	! variables in this <program>

		! the given natural number for this game
		INTEGER(KIND= I4B) :: target
		INTEGER(KIND= I4B) :: itr

		INTEGER(KIND= I4B), DIMENSION(:), ALLOCATABLE :: primefactors

	! support variables in this <program>
	INTEGER(KIND= I4B) :: statval

	! Main process is below

	READ( UNIT= *, FMT= *, IOSTAT= statval ) target
	CALL exception_read( VAL= statval, NAME= 'the given natural number for this game' )

	PRINT '(I12)', GetMaxPrimeFactor( target )

END PROGRAM main