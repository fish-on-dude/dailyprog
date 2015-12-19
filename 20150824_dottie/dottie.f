      PROGRAM dottie
      IMPLICIT NONE

      REAL X, STARTX

      INTEGER I
      LOGICAL CONVERGED


      X = 0.5
      CALL ITERATE(F1, X, CONVERGED)
      WRITE (*,1000)  'DOTTIE: ', X
 1000 FORMAT(A20, F9.5)
      X = 2
      CALL ITERATE(F2, X, CONVERGED)
      WRITE (*,1000) 'X - TAN X: ', X
      WRITE (*, *) 'FUNCTION 3: '
      DO I = 1, 10
         CALL RANDOM_NUMBER(STARTX)
         X = STARTX
         CALL ITERATE(F3, X, CONVERGED)
         IF (CONVERGED) THEN
            WRITE (*,1001) 'with a starting value of ', STARTX,
     $           ' the series converged to a value of ', X
 1001       FORMAT(A, F9.5, A, F9.5)
         ELSE
            WRITE (*,1001) 'with a starting value of ', STARTX,
     $           ' the series did not coverge'
         END IF
      END DO
      WRITE (*, *) 'FUNCTION 4: '
      DO I = 1, 10
         STARTX = I / 10.
         X = STARTX
         CALL ITERATE(F4, X, CONVERGED)
         IF (CONVERGED) THEN
            WRITE (*,1001) 'with a starting value of ', STARTX,
     $           ' the series converged to a value of ', X
         ELSE
            WRITE (*,1001) 'with a starting value of ', STARTX,
     $           ' the series did not coverge'
         END IF
      END DO
      
      CONTAINS
      REAL FUNCTION F1(X)
      REAL X
      F1 = COS(X)
      END FUNCTION
      
      REAL FUNCTION F2(X)
      REAL X
      F2 = X - TAN(X)
      END FUNCTION

      REAL FUNCTION F3(X)
      REAL X
      F3 = 1 + 1./X
      END FUNCTION

      REAL FUNCTION F4(X)
      REAL X
      F4 = 4*X*(1-X)
      END FUNCTION

      
      SUBROUTINE ITERATE(F, X, CONVERGED)
      
      REAL TEMPX, X, DEL,  F
      LOGICAL CONVERGED  
      
      INTEGER I
      INTEGER, PARAMETER :: MAXN = 100
      REAL, PARAMETER :: EPS = 1E-5

      CONVERGED = .FALSE.
      TEMPX = X
      DO 
         TEMPX = F(X)
         DEL = ABS(X - TEMPX)
         X = TEMPX
         IF (DEL .LE. EPS) THEN
            CONVERGED = .TRUE.
            EXIT
         ELSE
            I = I + 1
            IF (I .GT. MAXN) EXIT
         END IF 
      END DO

      END SUBROUTINE
      END PROGRAM
      
