!********************************************************************
!  Solution of system of linear equations by Gauss Jordan reduction
!********************************************************************
SUBROUTINE GAUJOR(a,wa,n,n1,det,e)

	! arguments for this <SUBROUTINE>

	INTEGER( KIND= 4 ), INTENT(IN) :: n  ! size of the coefficient matrix
	INTEGER( KIND= 4 ), INTENT(IN) :: n1 ! `size of the coefficient matrix` + 1

	DOUBLE PRECISION, DIMENSION(n,n1) :: a
	DOUBLE PRECISION, DIMENSION(n,n1) :: wa

	DOUBLE PRECISION, INTENT(INOUT) :: det ! determinant
	DOUBLE PRECISION, INTENT(INOUT) :: e   ! allowable error

	! local variables in this <SUBROUTINE>
	DOUBLE PRECISION   :: w          ! buffer for diagonal component
	INTEGER( KIND= 4 ) :: i,j,k,l    ! iterator
	LOGICAL( KIND= 1 ) :: canCmptDet ! can this <SUBROUTINE> calculate the determinant of the coefficient matrix

	! STEP.01
	! initialize the determinant
	det = 1.0D+0

	! STEP.02
	! initialize the allowable error
	e = MAX( e, 1.0D-5 )

	! STEP.03
	DO k=1,n

		w=a(k,k)
		IF( ABS(w).GT.e ) THEN
			l=k
			FORALL(j=k:n1)
				wa(j)=a(k,j); a(k,j)=a(l,j); a(l,j)=wa(j)
			ENDFORALL
			det=-det
		ELSE

		ENDIF

		! exchange rows
		FORALL(j=k:n1) wa(j)=a(k,j); a(k,j)=a(l,j); wa(l,j)=wa(j)

		! update the determinant
		det= -det; det=w*det

	ENDDO

END SUBROUTINE GAUJOR

! SUBROUTINE GAUJOR(a,wa,n,n1,det,e)
! 	DIMENSION a(n,n1), wa(n1)
! 	det=1.
! 	e=MAX(e,1.E-5)
! 	DO k=1,n
! 		w=a(k,k); IF(ABS(w)>e) GOTO 11
! 		l=k; 12 l=l+1; IF(l>n)STOP 5555 ! improper coef matrix
! 		w=a(l,k); IF(ABS(w)<=e) GOTO 12
! 		FORALL(j=k:n1) ! exchange rows
! 			wa(j)=a(k,j); a(k,j)=a(l,j); a(l,j)=wa(j)
! 		ENDFORALL
! 		det=-det; 11 det=w*det ! compute det(A)
! 		FORALL(j=k+1:n1) a(k,j)=a(k,j)/w
! 		cycle_1: DO i=1,n: IF(i==k) CYCLE cycle_1
! 			FORALL(j=k+1:n1) a(i,j)=a(i,j)-a(i,k)*a(k,j)
! 		ENDDO cycle_1
! 	ENDDO
! END