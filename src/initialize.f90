      SUBROUTINE initialize()
!!    Subroutine designed to initiate a new mesh.
!!      meshSize : INTEGER - Size of a mesh. 
!!                           We're only considering a square mesh.
!!      mesh : INTEGER(:,:) - Mesh

      USE global_variables

      IMPLICIT NONE

      INTEGER :: i, j
      DOUBLE PRECISION :: rand

      DO i = 1, meshSize
        DO j = 1, meshSize
          IF (rand() .LE. 0.50d0) THEN
            mesh(i, j) = 1
          ELSE
            mesh(i, j) = -1
          END IF

        END DO
      END DO

      END SUBROUTINE initialize

