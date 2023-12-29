      SUBROUTINE printMesh()
!!    Subroutine for printing current mesh onto a terminal.

      USE global_variables

      IMPLICIT NONE

      INTEGER :: i, j

      DO j = 1, meshSize
        WRITE(*,"(A)", ADVANCE = "no") "+----"
      END DO
      WRITE(*,"(A)") "+"

      DO i = 1, meshSize
        DO j = 1, meshSize
          WRITE (*,"(A,I3,A)", ADVANCE = "no") "|", mesh(i, j), " "
        END DO
        WRITE(*,"(A)") "|"
        DO j = 1, meshSize
          WRITE (*,"(A)", ADVANCE = "no") "+----"
        END DO
        WRITE(*,"(A)") "+"
      END DO

      END SUBROUTINE printMesh

      SUBROUTINE saveMesh(fileName)
!!    Subroutine for printing current mesh into a file "fileName"
!!      fileName : STRING - file name for output

      USE global_variables

      IMPLICIT NONE

      INTEGER :: i, j
      CHARACTER(len=256) :: fileName

      fileName = './dump/' // fileName
      OPEN(1000, FILE = TRIM(fileName), STATUS = 'NEW')

      DO i = 1, meshSize
        DO j = 1, meshSize
          IF (j .EQ. meshSize) THEN
            WRITE(1000, "(I4)", ADVANCE = "no") mesh(i, j)
          ELSE
            WRITE(1000, "(I4, A)", ADVANCE = "no") mesh(i, j), ","
          END IF
        END DO
        WRITE(1000,*)
      END DO

      CLOSE(1000)

      END SUBROUTINE saveMesh
