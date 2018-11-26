MODULE ProblemSetting

    ! modules to use
    USE nrtype
    
    ! Parameters
    INTEGER( KIND=I4B ), PARAMETER :: PointsX    = 201
    INTEGER( KIND=I4B ), PARAMETER :: PointsY    = PointsX
    INTEGER( KIND=I4B ), PARAMETER :: Substances = 3
    INTEGER( KIND=I4B ), PARAMETER :: TrgtSubst  = 2
    INTEGER( KIND=I4B ), PARAMETER :: TimeSteps  = 1000
    INTEGER( KIND=I4B ), PARAMETER :: OutputStep = 1
    REAL(    KIND=DPR ), PARAMETER :: SpaceStep  = 1.0D-2
    REAL(    KIND=DPR ), PARAMETER :: TimeStep   = 2.0D-3
    
    REAL( KIND=DPR ), DIMENSION(1:3), PARAMETER :: Velocity = (/ 2.0D+0, 1.0D+0, 1.0D+0 /)

    INTEGER( KIND=I4B ), PARAMETER :: UnitNumSave = 10
    
    CHARACTER( LEN= 128 ), PARAMETER :: path_fldr_save = 'hoge'
    CHARACTER( LEN= 128 ), PARAMETER :: name_file_save = 'step'
    CHARACTER( LEN=   8 ), PARAMETER :: extension_save = '.dat'
    
    ! support variables for this <MODULE>
    INTEGER( KIND=I4B ), PRIVATE :: statval
    INTEGER( KIND=I4B ), PRIVATE :: zero
    
    ! <SUBROUTINE> and <FUNCTIONS> in this <MODULE>
    CONTAINS



    ! get the zero which kind is same as 'statval'
    FUNCTION GetSameKindZero( TRGT ) RESULT( RTVL )

        ! arguments for this <FUNCTION>
        INTEGER( KIND=I4B ), INTENT(IN) :: TRGT
    
        RTVL = INT( 0, KIND=KIND(RTVL) )
        RETURN

    ENDFUNCTION


    ! allocation of the simulation field
    SUBROUTINE AllocationField( trgt, X, Y, S )

        ! arguments for this <SUBROUTINE>
        REAL( KIND=DPR ), DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: trgt ! target of allocation

        INTEGER( KIND=I4B ), INTENT(IN) :: X ! number of the points (x-direction)
        INTEGER( KIND=I4B ), INTENT(IN) :: Y ! number of the points (y-direction)
        INTEGER( KIND=I4B ), INTENT(IN) :: S ! number of the substances
        
        IF( ALLOCATED(trgt) ) THEN
            STOP 'target array had already allocated' ! BAD END
        ENDIF

        ALLOCATE( trgt(1:Y,1:X,1:S), STAT= statval )

        IF( statval .GT. GetSameKindZero(statval) ) THEN
            ! BAD END
            STOP 'failed to allocate the array'
        ELSEIF( statval .LT. GetSameKindZero(statval) ) THEN
            ! BAD END
            STOP 'undefined error detected in <ALLOCATE> statement'
        ENDIF
        
        RETURN ! TRUE END
        
    ENDSUBROUTINE



    ! deallocation of the simulation field
    SUBROUTINE DeallocationField( trgt )

        ! arguments for this <SUBROUTINE>
        REAL( KIND=DPR ), DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: trgt ! target of allocation

        IF( ALLOCATED(trgt) ) THEN
            DEALLOCATE( trgt, STAT= statval )
            IF( statval .GT. GetSameKindZero(statval) ) THEN
                ! BAD END
                STOP 'failed to deallocate the array'
            ENDIF
        ENDIF
        
        RETURN ! TRUE END
        
    ENDSUBROUTINE



    ! open the file to save the result
    SUBROUTINE OpenFileSave( unit, file )

        ! arguments for this <SUBROUTINE>
        INTEGER( KIND=I4B ), INTENT(IN) :: unit
        CHARACTER( LEN=* ),  INTENT(IN) :: file
        
        OPEN( &
            UNIT        = unit,                 &
            DEFAULTFILE = TRIM(path_fldr_save), &
            FILE        = TRIM(file),           &
            IOSTAT      = statval,              &
            ACTION      = 'WRITE',              &
            FORM        = "FORMATTED",          &
            STATUS      = 'REPLACE'             &
        )
        
        IF( statval .GT. GetSameKindZero(statval) ) THEN
            PRINT '(A)', 'failed to open the target file'
            PRINT '(A)', '[PATH]' // path_fldr_save // file
            STOP
        ENDIF
        
        RETURN ! TRUE END

    ENDSUBROUTINE



    ! open the file to save the result
    SUBROUTINE CloseFileSave( unit )

        ! arguments for this <SUBROUTINE>
        INTEGER( KIND=I4B ), INTENT(IN) :: unit
        
        CLOSE( UNIT= unit, STATUS= 'KEEP', IOSTAT= statval )
        
        IF( statval .GT. GetSameKindZero(statval) ) THEN
            PRINT '(A)', 'failed to close the target file'
            STOP
        ENDIF
        
        RETURN ! TRUE END

    ENDSUBROUTINE



    ! open the file to save the result
    SUBROUTINE SaveResultUnit( unit, trgt )

        ! arguments for this <SUBROUTINE>
        INTEGER( KIND=I4B ), INTENT(IN) :: unit
        
        REAL( KIND=DPR ), DIMENSION(:,:), INTENT(IN) :: trgt
        
        ! local variables for this <SUBROUTINE>
        INTEGER( KIND=I4B ) :: itr
        
        DO itr2 = 1,PointsX,1
            WRITE( UNIT= unit, FMT='(ES18.10E3,16384(" ",ES18.10E3))' ) trgt(1:PointsY:1,itr2)
        ENDDO
        
        RETURN ! TRUE END

    ENDSUBROUTINE

    SUBROUTINE SaveResult( unit, trgt )

        ! arguments for this <SUBROUTINE>
        INTEGER( KIND=I4B ), INTENT(IN) :: unit
        
        REAL( KIND=DPR ), DIMENSION(:,:,:), INTENT(IN) :: trgt
        
        ! local variables for this <SUBROUTINE>
        INTEGER( KIND=I4B ) :: itr
    
        CALL SaveResultUnit( UNIT= UnitNumSave, TRGT= trgt(:,:,TrgtSubst) )
        ! WRITE( UNIT= UnitNumSave, FMT='(A/)' ) ' '
        ! CALL SaveResultUnit( UNIT= UnitNumSave, TRGT= trgt(:,:,2) ); WRITE( UNIT= UnitNumSave, FMT='(A/)' ) ' '
        ! CALL SaveResultUnit( UNIT= UnitNumSave, TRGT= trgt(:,:,3) ); WRITE( UNIT= UnitNumSave, FMT='(A/)' ) ' '
        ! CALL SaveResultUnit( UNIT= UnitNumSave, TRGT= trgt(:,:,1)+trgt(:,:,2)+trgt(:,:,3) )
        
        RETURN ! TRUE END

    ENDSUBROUTINE

ENDMODULE



MODULE Simulation

    ! modules to use
    USE nrtype
    USE ProblemSetting
    USE MT95

    ! Require all variables to be explicitly declared
    IMPLICIT NONE
    
    ! constants for this <MODULE>
    REAL( KIND=DPR ), PARAMETER, PRIVATE :: SpaceStep2 = 1.0D+0 / SpaceStep * SpaceStep
    
    ! variables for this <MODULE>
    INTEGER( KIND=I4B ), DIMENSION(1:2), SAVE :: lwb_buff, upb_buff
    INTEGER( KIND=I4B ), DIMENSION(1:3), SAVE :: lwb_trgt, upb_trgt
    
    REAL( KIND= DPR ), DIMENSION(:,:,:), ALLOCATABLE, SAVE, PRIVATE :: laplacian, buff
    
    ! support variables for this <MODULE>
    INTEGER( KIND=I4B ), PRIVATE :: statval
    
    ! <SUBROUTINE> and <FUNCTIONS> in this <MODULE>
    CONTAINS
    
    ! prepare for simulation
    SUBROUTINE PrepareSimulation( trgt )

        ! arguments for this <SUBROUTINE>
        REAL( KIND=DPR ), DIMENSION(:,:,:), INTENT(INOUT) :: trgt ! simulation field
        
        ! local variables for this <SUBROUTINE>
        INTEGER( KIND=I4B ) :: itr
        
        ! STEP.01
        ! check the size of the arrays
        FORALL( itr=1:3:1 )
            lwb_trgt(itr) = LBOUND( trgt, DIM=itr )
            upb_trgt(itr) = UBOUND( trgt, DIM=itr )
        ENDFORALL

        ! STEP.02
        ! allocation
        ALLOCATE( laplacian( lwb_trgt(1)   : upb_trgt(1),    lwb_trgt(2)   : upb_trgt(2),    lwb_trgt(3) : upb_trgt(3) ), STAT= statval )
        ALLOCATE(      buff( lwb_trgt(1)-1 : upb_trgt(1)+1 , lwb_trgt(1)-1 : upb_trgt(1)+1 , lwb_trgt(3) : upb_trgt(3) ), STAT= statval )

        ! STEP.02
        ! check the size of the arrays
        FORALL( itr=1:2:1 )
            lwb_buff(itr) = LBOUND( buff, DIM=itr )
            upb_buff(itr) = UBOUND( buff, DIM=itr )
        ENDFORALL
        
        RETURN ! TRUE END
        
    ENDSUBROUTINE



    ! finishing simulation
    SUBROUTINE FinishSimulation
        CALL DeallocationField( TRGT= laplacian )
        CALL DeallocationField( TRGT= buff )
        RETURN
    ENDSUBROUTINE
    
    
    
    ! normalize
    SUBROUTINE Normalization( trgt )

        ! arguments for this <SUBROUTINE>
        REAL( KIND=DPR ), DIMENSION(:,:,:), INTENT(INOUT) :: trgt ! target of allocation
        
        ! local variables for this <SUBROUTINE>
        INTEGER( KIND=I4B ) :: itr1, itr2, itr3

        FORALL( itr1=lwb_trgt(1):upb_trgt(1):1, itr2=lwb_trgt(2):upb_trgt(2):1, itr3=lwb_trgt(3):upb_trgt(3):1 )
            trgt(itr1,itr2,itr3) = MIN( 1.0D+0, MAX( 0.0D+0, trgt(itr1,itr2,itr3) ) )
            ! trgt(itr1,itr2,itr3) = ( TANH( trgt(itr1,itr2,itr3) ) + 1.0D+0 ) * 5.0D-1
        ENDFORALL
        
        FORALL( itr1=lwb_trgt(1):upb_trgt(1):1, itr2=lwb_trgt(2):upb_trgt(2):1 )
            buff(itr1,itr2,1) = trgt(itr1,itr2,lwb_trgt(3))
        ENDFORALL

        DO itr3=lwb_trgt(3),upb_trgt(3),1
            FORALL( itr1=lwb_trgt(1):upb_trgt(1):1, itr2=lwb_trgt(2):upb_trgt(2):1 )
                buff(itr1,itr2,1) = buff(itr1,itr2,1) + trgt(itr1,itr2,itr3)
            ENDFORALL
        ENDDO

        ! buff(:,:,1) = 1.0D+0 / buff(:,:,1)

        DO itr3=lwb_trgt(3),upb_trgt(3),1
            FORALL( itr1=lwb_trgt(1):upb_trgt(1):1, itr2=lwb_trgt(2):upb_trgt(2):1 )
                trgt(itr1,itr2,itr3) = trgt(itr1,itr2,itr3) / buff(itr1,itr2,1)
            ENDFORALL
        ENDDO

        RETURN ! TRUE END
        
    ENDSUBROUTINE



    ! set the initial condition
    SUBROUTINE SetInitialCondition( trgt, put )

        ! arguments for this <SUBROUTINE>
        REAL( KIND=DPR ), DIMENSION(:,:,:), INTENT(INOUT) :: trgt ! target of allocation
        
        INTEGER( KIND=I4B ), INTENT(IN) :: put ! seed value for SUBROUTINE <MT95>
        
        CALL GENRAND_INIT( PUT= put ) ! set the seed value
        CALL GENRAND_REAL1( trgt )    ! set the initial condition [0:1]
        CALL Normalization( trgt )
        
        RETURN ! TRUE END
        
    ENDSUBROUTINE



    ! calculate laplacian
    SUBROUTINE CalcLaplacian( trgt )

        ! arguments for this <SUBROUTINE>
        REAL( KIND=DPR ), DIMENSION(:,:,:), INTENT(IN) :: trgt ! target to calculate laplacian
        
        ! support variables for this <SUBROUTINE>
        INTEGER( KIND=I4B ) :: itr1, itr2, itr3

        ! STEP.02
        ! make the buffer to calculate the laplacian
        DO itr3= lwb_trgt(3),upb_trgt(3),1
        
            ! set the boundary condition (periodical condition)
            FORALL( itr1=lwb_trgt(1):upb_trgt(1):1 )
                buff(itr1,lwb_buff(2),itr3) = trgt(itr1,upb_trgt(2),itr3)
                buff(itr1,upb_buff(2),itr3) = trgt(itr1,lwb_trgt(2),itr3)
            ENDFORALL

            FORALL( itr2=lwb_trgt(2):upb_trgt(2):1 )
                buff(lwb_buff(1),itr2,itr3) = trgt(upb_trgt(1),itr2,itr3)
                buff(upb_buff(1),itr2,itr3) = trgt(lwb_trgt(1),itr2,itr3)
            ENDFORALL
            
            ! set the internal area
            FORALL( & !
                itr1=lwb_trgt(1):upb_trgt(1):1, & !
                itr2=lwb_trgt(2):upb_trgt(2):1 )
                buff(itr1,itr2,itr3) = trgt(itr1,itr2,itr3)
            ENDFORALL
            
        ENDDO
        
        ! STEP.03
        ! calculate the laplacian
        DO itr3 = lwb_trgt(3),upb_trgt(3),1

            FORALL( & !
                itr1 = lwb_trgt(1):upb_trgt(1):1, & !
                itr2 = lwb_trgt(2):upb_trgt(2):1  )
                laplacian(itr1,itr2,itr3) = -4.0D+0 * buff(itr1,itr2,itr3)
            ENDFORALL

            FORALL( & !
                itr1 = lwb_trgt(1):upb_trgt(1):1, & !
                itr2 = lwb_trgt(2):upb_trgt(2):1  )
                laplacian(itr1,itr2,itr3) = laplacian(itr1,itr2,itr3) + buff(itr1-1,itr2,itr3)
            ENDFORALL

            FORALL( & !
                itr1 = lwb_trgt(1):upb_trgt(1):1, & !
                itr2 = lwb_trgt(2):upb_trgt(2):1  )
                laplacian(itr1,itr2,itr3) = laplacian(itr1,itr2,itr3) + buff(itr1+1,itr2,itr3)
            ENDFORALL

            FORALL( & !
                itr1 = lwb_trgt(1):upb_trgt(1):1, & !
                itr2 = lwb_trgt(2):upb_trgt(2):1  )
                laplacian(itr1,itr2,itr3) = laplacian(itr1,itr2,itr3) + buff(itr1,itr2-1,itr3)
            ENDFORALL

            FORALL( & !
                itr1 = lwb_trgt(1):upb_trgt(1):1, & !
                itr2 = lwb_trgt(2):upb_trgt(2):1  )
                laplacian(itr1,itr2,itr3) = laplacian(itr1,itr2,itr3) + buff(itr1,itr2+1,itr3)
            ENDFORALL

            FORALL( & !
                itr1 = lwb_trgt(1):upb_trgt(1):1, & !
                itr2 = lwb_trgt(2):upb_trgt(2):1  )
                laplacian(itr1,itr2,itr3) = laplacian(itr1,itr2,itr3) * SpaceStep2
            ENDFORALL
            
        ENDDO
        
        RETURN ! TRUE END
        
    ENDSUBROUTINE
    
    
    
    SUBROUTINE CalcTimeStep( trgt )

        ! arguments for this <SUBROUTINE>
        REAL( KIND=DPR ), DIMENSION(:,:,:), INTENT(INOUT) :: trgt
        
        ! support variables for this <SUBROUTINE>
        INTEGER( KIND=I4B ) :: itr1, itr2, itr3
    
        ! STEP.01
        ! calculate the laplacian of the concentration
        CALL CalcLaplacian( trgt )
        CALL CalcLaplacian( laplacian )
        
        ! STEP.02
        ! update the concentration field
        FORALL( & !
            itr1 = lwb_trgt(1):upb_trgt(1):1, & !
            itr2 = lwb_trgt(2):upb_trgt(2):1  )
            buff(itr1,itr2,1) = Velocity(2)*laplacian(itr1,itr2,2) - Velocity(3)*laplacian(itr1,itr2,3)
            buff(itr1,itr2,2) = Velocity(3)*laplacian(itr1,itr2,3) - Velocity(1)*laplacian(itr1,itr2,1)
            buff(itr1,itr2,3) = Velocity(1)*laplacian(itr1,itr2,1) - Velocity(2)*laplacian(itr1,itr2,2)
        ENDFORALL
        
        FORALL( & !
            itr1 = lwb_trgt(1):upb_trgt(1):1, & !
            itr2 = lwb_trgt(2):upb_trgt(2):1, & !
            itr3 = lwb_trgt(3):upb_trgt(3):1  )
            trgt(itr1,itr2,itr3) = trgt(itr1,itr2,itr3) + TimeStep * buff(itr1,itr2,itr3)
        ENDFORALL
        
        CALL Normalization( trgt )
        
        RETURN ! TRUE END
        
    ENDSUBROUTINE
    
    SUBROUTINE CheckNaN( trgt )

        ! arguments for this <SUBROUTINE>
        REAL( KIND=DPR ), DIMENSION(:,:,:), INTENT(INOUT) :: trgt
        
        ! local variables for this <SUBROUTINE>
        LOGICAL( KIND=LGT ) :: isthereNAN
        
        ! support variables for this <SUBROUTINE>
        INTEGER( KIND=I4B ) :: itr1, itr2, itr3
        
        ! STEP.01
        ! initialize the flag
        isthereNAN = .FALSE.
        
        ! STEP.02
        ! search the Not a Number
        DO itr3 = lwb_trgt(3),upb_trgt(3),1
        DO itr2 = lwb_trgt(2),upb_trgt(2),1
        DO itr1 = lwb_trgt(1),upb_trgt(1),1
            isthereNAN = FuncISNAN( trgt(itr1,itr2,itr3) )
        ENDDO
        ENDDO
        ENDDO
        
        IF( isthereNAN ) STOP "`Not a Number` was detected!"
        
        RETURN ! TRUE END
        
    ENDSUBROUTINE
    
    PURE FUNCTION FuncISNAN( trgt )

        ! arguments for this <SUBROUTINE>
        REAL( KIND=DPR ), INTENT(IN) :: trgt

        ! return value for this <SUBROUTINE>
        LOGICAL( KIND=LGT ) :: FuncISNAN
        
        IF( ISNAN(trgt) ) THEN
            FuncISNAN = .TRUE.; RETURN
        ELSE
            FuncISNAN = .FALSE.; RETURN
        ENDIF
        
    ENDFUNCTION

ENDMODULE



PROGRAM MAIN

    ! modules to use
    USE nrtype
    USE Simulation
    USE ProblemSetting

    ! Require all variables to be explicitly declared
    IMPLICIT NONE
    
    ! concentration field
    REAL( KIND=DPR ), DIMENSION(:,:,:), ALLOCATABLE :: concentration
    
    ! support variables for this <PROGRAM>
    INTEGER( KIND=I4B ) :: statval, itr
    CHARACTER( LEN=10 ) :: str_step
    
    ! STEP.01
    ! allocation
    CALL AllocationField( TRGT= concentration, X= PointsX, Y= PointsY, S= Substances )
    
    ! STEP.02
    ! set the initial condition
    
    ! STEP.03
    ! simulation
    CALL PrepareSimulation( concentration )
    CALL SetInitialCondition( TRGT= concentration, PUT= 1 )
    
    DO itr = 0,TimeSteps,1
        IF ( MOD( itr, OutputStep ) .EQ. 0 .AND. itr .GE. 800 ) THEN
            WRITE( UNIT= str_step, FMT= '(I6.6)' ) , itr
            CALL OpenFileSave( UNIT= UnitNumSave, FILE= TRIM(name_file_save) // TRIM(str_step) // extension_save )
            CALL SaveResult( UNIT= UnitNumSave, TRGT= concentration(:,:,:) )
            CALL CloseFileSave( UNIT= UnitNumSave )
        ENDIF
        CALL CheckNaN( concentration )
        CALL CalcTimeStep( concentration )
    ENDDO

    CALL FinishSimulation
    
    ! STEP.04
    ! deallocation
    CALL DeallocationField( TRGT= concentration )
    
    ! STEP.TRUE END
    PRINT '(A)', '[TRUE END]'

ENDPROGRAM