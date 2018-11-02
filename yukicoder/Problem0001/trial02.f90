! yukicoder Problem 0001
! 
! code created : 2018.11.03
! code updated : 2018.11.03
!
! compiler : Fortran (gFortran 4.8.5)
!
! [reference]
! http://www.deqnotes.net/acmicpc/dijkstra/

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
INTEGER, PARAMETER :: LGT = KIND(.true.)

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
	CHARACTER(LEN= *),  INTENT(IN) :: name ! name of the target array

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

PROGRAM main

	! <MODULE> to use
	USE nrtype
	USE Exception

	! Require all variables to be explicitly declared
	IMPLICIT NONE

	! variables for this <PROGRAM>
	INTEGER(KIND= I4B) :: num_cities ! the number of cities
	INTEGER(KIND= I4B) :: lim_cost   ! the limit of cost
	INTEGER(KIND= I4B) :: num_routes ! the number of routes

	TYPE route
		INTEGER(KIND= I4B) :: start ! index of city which is start of the route
		INTEGER(KIND= I4B) :: goal  ! index of city which is goal  of the route
		INTEGER(KIND= I4B) :: cost  ! cost to go through the route
		INTEGER(KIND= I4B) :: time  ! time to go through the route
	END TYPE

	TYPE(route), DIMENSION(:), ALLOCATABLE :: routes

	! support variables for this <PROGRAM>
	INTEGER(KIND= I4B) :: statval

	! STEP.01
	! get the setting of the problem from starndard input
	READ( UNIT=*, FMT=*, IOSTAT= statval ) num_cities
	CALL exception_read( VAL= statval, NAME='num_cities' )

	READ( UNIT=*, FMT=*, IOSTAT= statval ) lim_cost
	CALL exception_read( VAL= statval, NAME='lim_cost' )

	READ( UNIT=*, FMT=*, IOSTAT= statval ) num_routes
	CALL exception_read( VAL= statval, NAME='num_routes' )

	ALLOCATE( routes(1:num_routes), STAT= statval )
	CALL exception_allocalte( VAL= statval, NAME= 'routes' )

	READ( UNIT=*, FMT=*, IOSTAT= statval ) routes(:)%start
	CALL exception_read( VAL= statval, NAME='routes%start' )

	READ( UNIT=*, FMT=*, IOSTAT= statval ) routes(:)%goal
	CALL exception_read( VAL= statval, NAME='routes%goal' )

	READ( UNIT=*, FMT=*, IOSTAT= statval ) routes(:)%cost
	CALL exception_read( VAL= statval, NAME='routes%cost' )

	READ( UNIT=*, FMT=*, IOSTAT= statval ) routes(:)%time
	CALL exception_read( VAL= statval, NAME='routes%time' )

	PRINT     '(I5)', num_cities
	PRINT     '(I5)', lim_cost
	PRINT     '(I5)', num_routes
	PRINT   '(50I5)', routes(:)%start
	PRINT   '(50I5)', routes(:)%goal
	PRINT  '(300I5)', routes(:)%cost
	PRINT '(1000I5)', routes(:)%time

	! STEP.02
	! declaration
	DEALLOCATE( routes, STAT= statval )
	CALL exception_deallocalte( VAL= statval, NAME= 'routes')

END PROGRAM main