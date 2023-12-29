!       _____ _                _____ _____ _____ ______ _____ ______ _____  
!      / ____| |        /\    / ____/ ____|_   _|  ____|_   _|  ____|  __ \ 
!     | |    | |       /  \  | (___| (___   | | | |__    | | | |__  | |  | |
!     | |    | |      / /\ \  \___ \\___ \  | | |  __|   | | |  __| | |  | |
!     | |____| |____ / ____ \ ____) |___) |_| |_| |     _| |_| |____| |__| |
!      \_____|______/_/    \_\_____/_____/|_____|_|    |_____|______|_____/ 
!                                                                           
!         This file is for teachers only!

      SUBROUTINE totalEnergy(totEng)

        USE global_variables

        IMPLICIT NONE

        INTEGER :: i, j
        DOUBLE PRECISION :: E, totEng

        totEng = 0.d0
        DO i = 1, meshSize
          DO j = 1, meshSize
            CALL hamiltonian(i, j, E)
            totEng = totEng + E
          END DO
        END DO

      END SUBROUTINE totalEnergy

      SUBROUTINE swap(i, j, T)

        USE global_variables

        IMPLICIT NONE

        INTEGER :: i, j
        DOUBLE PRECISION :: T
        DOUBLE PRECISION :: prop, rand
        DOUBLE PRECISION :: before, after, dE

        CALL hamiltonian(i, j, before)
        mesh(i, j) = -1 * mesh(i, j)
        CALL hamiltonian(i, j, after)
!       Revert flipping
        mesh(i, j) = -1 * mesh(i, j)

        dE = after - before

        IF (dE .LE. 0) THEN
          mesh(i, j) = -1 * mesh(i, j)
        ELSE
          prop = DEXP(-1.d0 * dE / (DBLE(k) * T))
          IF (rand() .LE. prop) THEN
            mesh(i, j) = -1 * mesh(i, j)
          END IF
        END IF

      END SUBROUTINE swap

      SUBROUTINE hamiltonian(i, j, E)

        USE global_variables

        IMPLICIT NONE

        INTEGER :: i, j
        INTEGER :: sigma
        DOUBLE PRECISION :: E

        sigma = 0

        IF (i .EQ. meshSize) THEN
          sigma = sigma + (coupling * mesh(i, j) * mesh(1, j)) 
          sigma = sigma + (coupling * mesh(i, j) * mesh(i-1, j)) 
        ELSE IF (i .EQ. 1) THEN
          sigma = sigma + (coupling * mesh(i, j) * mesh(i+1, j)) 
          sigma = sigma + (coupling * mesh(i, j) * mesh(meshSize, j)) 
        ELSE
          sigma = sigma + (coupling * mesh(i, j) * mesh(i+1, j)) 
          sigma = sigma + (coupling * mesh(i, j) * mesh(i-1, j)) 
        END IF

        IF (j .EQ. meshSize) THEN
          sigma = sigma + (coupling * mesh(i, j) * mesh(i, 1)) 
          sigma = sigma + (coupling * mesh(i, j) * mesh(i, j-1)) 
        ELSE IF (j .EQ. 1) THEN
          sigma = sigma + (coupling * mesh(i, j) * mesh(i, j+1)) 
          sigma = sigma + (coupling * mesh(i, j) * mesh(i, meshSize)) 
        ELSE
          sigma = sigma + (coupling * mesh(i, j) * mesh(i, j+1)) 
          sigma = sigma + (coupling * mesh(i, j) * mesh(i, j-1)) 
        END IF

        E = -1.d0 * DBLE(sigma)

      END SUBROUTINE hamiltonian

      SUBROUTINE magnetizationCalc(totMag)

        USE global_variables

        IMPLICIT NONE

        INTEGER :: i, j, totalSum
        DOUBLE PRECISION :: totMag

        totalSum = 0
        DO i = 1, meshSize
          DO j = 1, meshSize
            totalSum = totalSum + mesh(i, j)
          END DO
        END DO

        totMag = DBLE(totalSum) / (DBLE(meshSize) * DBLE(meshSize))

      END SUBROUTINE magnetizationCalc
