      SUBROUTINE JACOBI(ANS,RHS,NN,MM,P,Q, ME,ME_P,ME_Q, TOL)
      IMPLICIT NONE
      INTEGER NN,MM,P,Q, ME,ME_P,ME_Q
      REAL    ANS(0:NN+1,0:MM+1)[0:P-1,0:*], &
              RHS(0:NN+1,0:MM+1)[0:P-1,0:*]
      REAL    TOL
!
!     Solve Helmholtz's equation using Jacobi iteration.
!
!     See: http://www.netlib.org/linalg/html_templates/Templates.html
!          R. Barrett et. al. (1994).  Templates for the solution of
!          Linear Systems: Building Blocks for Iterative Methods, 2nd
!          Edition.  SIAM.  Philadelphia, PA.
!
!     Equation solved is the 5-point stencil:
!           -1
!        -1  6 -1
!           -1
!     over a doubly periodic region.
!
!     SPMD domain decomposition - each image "owns" a (1:NN,1:MM) piece
!     of the (1:NN*P,1:MM*Q) array which is therefore distributed across
!     all images.  A halo is added around (1:NN,1:MM), so computations
!     can always use local arrays.
!
      INTEGER I,ITERATIONS,J,ME_PM,ME_PP,ME_QM,ME_QP,NEIGHBORS(5)
      REAL    PMAXI,RESID_MAX
      REAL    WRK(1:NN,1:MM)
      REAL, SAVE :: PMAX[*]

      ME_QP = MOD(ME_Q+1+Q,Q)  ! north
      ME_QM = MOD(ME_Q-1+Q,Q)  ! south
      ME_PP = MOD(ME_P+1+P,P)  ! east
      ME_PM = MOD(ME_P-1+P,P)  ! west
      IF (MIN(P,Q) >= 3) THEN  ! list of neighboring images
        NEIGHBORS = (/ ME, ME_P  + 1 + P*ME_QM, ME_P  + 1 + P*ME_QP, &
                           ME_PM + 1 + P*ME_Q,  ME_PP + 1 + P*ME_Q   /)
      ENDIF

      DO ITERATIONS= 1,999
!
!       update halo.
!
        IF (MIN(P,Q) >= 3) THEN
          SYNC IMAGES( NEIGHBORS )
          !SYNC ALL
        ELSE
          SYNC ALL
        ENDIF  ! neighboring images have ANS(1:NN,1:MM) up to date
        ANS(1:NN,MM+1) = ANS(1:NN,1   )[ME_P, ME_QP]  ! north
        ANS(1:NN,   0) = ANS(1:NN,  MM)[ME_P, ME_QM]  ! south
        ANS(NN+1,1:MM) = ANS(1,   1:MM)[ME_PP,ME_Q ]  ! east
        ANS(   0,1:MM) = ANS(  NN,1:MM)[ME_PM,ME_Q ]  ! west
!
!       5-point stencil is correct everywhere, since halo is up to date.
!
        DO J= 1,MM
          DO I= 1,NN
            WRK(I,J) = (1.0/6.0) * (RHS(I,  J  ) + &
                                    ANS(I-1,J  ) + &
                                    ANS(I+1,J  ) + &
                                    ANS(I,  J-1) + &
                                    ANS(I,  J+1)   )
          ENDDO
        ENDDO
!
!       calculate global maximum residual error.
!
        PMAX = MAXVAL( ABS( WRK(1:NN,1:MM) - ANS(1:NN,1:MM) ) )
        SYNC ALL ! protects both PMAX and ANS
        IF (ME == 1) THEN
          DO I= 2,NUM_IMAGES()
            PMAXI = PMAX[I]
            PMAX  = MAX( PMAX, PMAXI )
          ENDDO
        ENDIF
        !CALL SYNC_ALL( WAIT=(/1/) )  ! protects PMAX[1]
        IF (ME == 1) THEN
            SYNC IMAGES( * )
        ELSE
            SYNC IMAGES( 1 )
        ENDIF
        RESID_MAX = PMAX[1]
!
!       update the result, note that above SYNC_ALL() guarentees that
!       the old ANS(1:NN,1:MM) is no longer needed for halo update.
!
        ANS(1:NN,1:MM) = WRK(1:NN,1:MM)
!
!       exit if converged.
!
        IF (RESID_MAX <= TOL) THEN
          EXIT
        ENDIF
      ENDDO
      IF (ME == 1) THEN
        WRITE(6,"('After',I4,' iterations, maximum residual is',F12.8)") &
           ITERATIONS,PMAX
!        CALL SYNC_FILE(6)
           CALL FLUSH(6)
      ENDIF
      RETURN
      END SUBROUTINE JACOBI

      PROGRAM TEST_JACOBI
      IMPLICIT NONE
!
! Example implementation of Jacobi iterative method for Helmholtz's equation
! using SPMD 2-D domain decomposition and Co-Array Fortran.  Note that
! Jacobi has very poor convergence properties, but it is scalable and easy
! to understand and program.  Use a better scheme, such as Red-Black SOR or
! Preconditioned Conjugate Gradients, for solving real problems.
!
! See: http://www.netlib.org/linalg/html_templates/Templates.html
!      R. Barrett et. al. (1994).  Templates for the solution of
!      Linear Systems: Building Blocks for Iterative Methods, 2nd
!      Edition.  SIAM.  Philadelphia, PA.
!
! Equation solved is the 5-point stencil:
!       -1
!    -1  6 -1
!       -1
! over a doubly periodic region with a point source right hand side.
!
! Input: N,M,P,Q
!
!        N = 1st dimension of entire rectangular grid
!        M = 2nd dimension of entire rectangular grid
!        P = number of nodes in 1st dimension, N/P an integer
!        Q = number of nodes in 2nd dimension, M/Q an integer
!        Total number of nodes = P*Q, must be NUM_IMAGES()
!
! Correctness is confirmed by printing the solution for two locations
! of the point source on the right hand side (solutions should be
! identical after a shift).
!
! Alan. J. Wallcraft, Naval Research Laboratory, October 1998.
!
      INTERFACE  ! needed because jacobi has co-array dummy arguments
         SUBROUTINE JACOBI(ANS,RHS,NN,MM,P,Q, ME,ME_P,ME_Q, TOL)
         INTEGER NN,MM,P,Q, ME,ME_P,ME_Q
         REAL    ANS(0:NN+1,0:MM+1)[0:P-1,0:*], &
                 RHS(0:NN+1,0:MM+1)[0:P-1,0:*]
         REAL    TOL
         END SUBROUTINE JACOBI
      END INTERFACE

      INTEGER(KIND=8) :: ST, ET, RES
      REAL(KIND=8)    :: TT,RTMP

      INTEGER :: NARGS
      INTEGER :: N,M,P,Q
      INTEGER :: I1,II1,IP,IQ,J1,JJ1,ME,ME_P,ME_Q,MM,NN
      INTEGER :: TICKS1, TICKS2, START_TIME, END_TIME, RATE
      INTEGER, SAVE     :: IN(4)[*]
      REAL, ALLOCATABLE :: ANS(:,:)[:,:],RHS(:,:)[:,:]
      REAL, ALLOCATABLE :: GLOBAL_ANS(:,:)
      CHARACTER (LEN=20) :: BUFFER

      ME = THIS_IMAGE()

      IF (ME == 1) THEN
          NARGS = COMMAND_ARGUMENT_COUNT()
          IF (NARGS == 0) THEN
             WRITE(*,*) 'Solve Helmholtz equation via Jacobi iteration'
             WRITE(*,*)'N = 1st dimension of entire rectangular grid'
             WRITE(*,*)'M = 2nd dimension of entire rectangular grid'
             WRITE(*,*)'P = number of nodes in 1st dimension, N/P an integer'
             WRITE(*,*)'Q = number of nodes in 2nd dimension, M/Q an integer'
             WRITE(*,*)'Total number of nodes = P*Q, must be NUM_IMAGES()'
             WRITE(*,'(a)',advance='no') 'Enter N, M, P, and Q: '
             READ(5,*) IN(1:4)

             ! Fill up IN(:) with a sample input.
             !IN(1:4) = (/1000,1000,1,(NUM_IMAGES()/1)/)
          ELSE
            call getarg(1,BUFFER)
            READ(BUFFER,*) IN(1)
            call getarg(2,BUFFER)
            READ(BUFFER,*) IN(2)
            call getarg(3,BUFFER)
            READ(BUFFER,*) IN(3)
            call getarg(4,BUFFER)
            READ(BUFFER,*) IN(4)
          END IF
      END IF

       IF (ME == 1) THEN
           SYNC IMAGES( * )
       ELSE
           SYNC IMAGES( 1 )
       ENDIF

      N = IN(1)[1]
      M = IN(2)[1]
      P = IN(3)[1]
      Q = IN(4)[1]
      IF ((N/P)*P /= N .OR. (M/Q)*Q /= M .OR. P*Q /= NUM_IMAGES()) THEN
        IF (ME == 1) THEN
          WRITE(6,*) 'BAD INPUT VALUES'
          CALL FLUSH(6)
        ENDIF
        SYNC ALL
        STOP
      ENDIF
      NN   = N/P
      MM   = M/Q
      ME_Q = (ME-1)/P         ! ANS(1:NN,1:MM)[ME_P,ME_Q] holds
      ME_P = (ME-1) - ME_Q*P  ! the local piece of the global array
      SYNC ALL

!
!     allocate arrays and co-arrays.
!
      IF (ME == 1) THEN
        ALLOCATE(GLOBAL_ANS(N,M))
      ENDIF
      ALLOCATE(ANS(0:NN+1,0:MM+1)[0:P-1,0:*])  ! include a halo
      ALLOCATE(RHS(0:NN+1,0:MM+1)[0:P-1,0:*])  ! around (1:NN,1:MM)

!
!     point source in center of global array.
!
      IF (ME == 1) THEN
          CALL SYSTEM_CLOCK(START_TIME, RATE)
      ENDIF
      I1  = N/2
      J1  = M/2
      II1 = I1 - ME_P*NN
      JJ1 = J1 - ME_Q*MM
      RHS(:,:) = 0.0  ! rsh is mostly zero
      IF (II1 >= 1 .AND. II1 <= NN .AND. &
          JJ1 >= 1 .AND. JJ1 <= MM       ) THEN
        RHS(II1,JJ1) = 1.0  ! with single non-zero element
      ENDIF
      ANS(:,:) = 0.0  ! 1st guess answer is zero
      CALL JACOBI(ANS,RHS,NN,MM,P,Q, ME,ME_P,ME_Q, 1.E-6)
      SYNC ALL
      IF (ME == 1) THEN
        DO IQ= 0,Q-1
          DO IP= 0,P-1
            GLOBAL_ANS(IP*NN+1:IP*NN+NN,IQ*MM+1:IQ*MM+MM) = &
                     ANS(1:NN,1:MM)[IP,IQ]
          ENDDO
        ENDDO
        CALL SYSTEM_CLOCK(END_TIME)

        TICKS1=END_TIME-START_TIME
        WRITE(6,*) 'Point source in center of global array'
        WRITE(6,"('ANS.I = ',9F8.5)") &
           GLOBAL_ANS(I1,MOD(J1-1+M-M/8,M)+1), &
           GLOBAL_ANS(I1,MOD(J1-1+M-5,  M)+1), &
           GLOBAL_ANS(I1,MOD(J1-1+M-3,  M)+1), &
           GLOBAL_ANS(I1,MOD(J1-1+M-1,  M)+1), &
           GLOBAL_ANS(I1,J1), &
           GLOBAL_ANS(I1,MOD(J1-1+M+1,  M)+1), &
           GLOBAL_ANS(I1,MOD(J1-1+M+3,  M)+1), &
           GLOBAL_ANS(I1,MOD(J1-1+M+5,  M)+1), &
           GLOBAL_ANS(I1,MOD(J1-1+M+M/8,M)+1)
        WRITE(6,"('ANS.J = ',9F8.5)") &
           GLOBAL_ANS(MOD(I1-1+N-N/8,N)+1,J1), &
           GLOBAL_ANS(MOD(I1-1+N-5,  N)+1,J1), &
           GLOBAL_ANS(MOD(I1-1+N-3,  N)+1,J1), &
           GLOBAL_ANS(MOD(I1-1+N-1,  N)+1,J1), &
           GLOBAL_ANS(I1,J1), &
           GLOBAL_ANS(MOD(I1-1+N+1,  N)+1,J1), &
           GLOBAL_ANS(MOD(I1-1+N+3,  N)+1,J1), &
           GLOBAL_ANS(MOD(I1-1+N+5,  N)+1,J1), &
           GLOBAL_ANS(MOD(I1-1+N+N/8,N)+1,J1)
           CALL FLUSH(6)
      ENDIF


      IF (ME == 1) THEN
          SYNC IMAGES( * )
      ELSE
          SYNC IMAGES( 1 )
      ENDIF

      IF (ME == 1) THEN
          CALL SYSTEM_CLOCK(START_TIME, RATE)
      ENDIF
!
!     point source at first point of global array.
!
      I1  = 1
      J1  = 1
      II1 = I1 - ME_P*NN
      JJ1 = J1 - ME_Q*MM
      RHS(:,:) = 0.0  ! rsh is mostly zero
      IF (II1 >= 1 .AND. II1 <= NN .AND. &
          JJ1 >= 1 .AND. JJ1 <= MM       ) THEN
        RHS(II1,JJ1) = 1.0  ! with single non-zero element
      ENDIF
      ANS(:,:) = 0.0  ! 1st guess answer is zero
      CALL JACOBI(ANS,RHS,NN,MM,P,Q, ME,ME_P,ME_Q, 1.E-6)
      SYNC ALL
      if (ME == 1) THEN
      ENDIF
      IF (ME == 1) THEN
        DO IQ= 0,Q-1
          DO IP= 0,P-1
            GLOBAL_ANS(IP*NN+1:IP*NN+NN,IQ*MM+1:IQ*MM+MM) = &
                     ANS(1:NN,1:MM)[IP,IQ]
          ENDDO
        ENDDO

        CALL SYSTEM_CLOCK(END_TIME)
        TICKS2=END_TIME-START_TIME

        WRITE(6,*) 'Point source at first point of global array'
        WRITE(6,"('ANS.I = ',9F8.5)") &
           GLOBAL_ANS(I1,MOD(J1-1+M-M/8,M)+1), &
           GLOBAL_ANS(I1,MOD(J1-1+M-5,  M)+1), &
           GLOBAL_ANS(I1,MOD(J1-1+M-3,  M)+1), &
           GLOBAL_ANS(I1,MOD(J1-1+M-1,  M)+1), &
           GLOBAL_ANS(I1,J1), &
           GLOBAL_ANS(I1,MOD(J1-1+M+1,  M)+1), &
           GLOBAL_ANS(I1,MOD(J1-1+M+3,  M)+1), &
           GLOBAL_ANS(I1,MOD(J1-1+M+5,  M)+1), &
           GLOBAL_ANS(I1,MOD(J1-1+M+M/8,M)+1)
        WRITE(6,"('ANS.J = ',9F8.5)") &
           GLOBAL_ANS(MOD(I1-1+N-N/8,N)+1,J1), &
           GLOBAL_ANS(MOD(I1-1+N-5,  N)+1,J1), &
           GLOBAL_ANS(MOD(I1-1+N-3,  N)+1,J1), &
           GLOBAL_ANS(MOD(I1-1+N-1,  N)+1,J1), &
           GLOBAL_ANS(I1,J1), &
           GLOBAL_ANS(MOD(I1-1+N+1,  N)+1,J1), &
           GLOBAL_ANS(MOD(I1-1+N+3,  N)+1,J1), &
           GLOBAL_ANS(MOD(I1-1+N+5,  N)+1,J1), &
           GLOBAL_ANS(MOD(I1-1+N+N/8,N)+1,J1)
           CALL FLUSH(6)
      ENDIF

      if (ME == 1) THEN
          SYNC IMAGES( * )
        write(*, '(//A20,I8,A)')   "clock rate = ", RATE, " ticks/s"
        write(*, '(A20,I8)')           "ticks  = ", (TICKS1+TICKS2)
        write(*, '(A20,F8.2,A)') "elapsed time = ", &
    & (TICKS1+TICKS2)/(1.0*rate), " seconds"
      ELSE
          SYNC IMAGES( 1 )
      ENDIF


      END PROGRAM TEST_JACOBI

