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