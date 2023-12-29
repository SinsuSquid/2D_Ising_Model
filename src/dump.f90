      SUBROUTINE perEnsemble_dump(T, dumpFreq)

        USE global_variables

        IMPLICIT NONE

        INTEGER :: dumpFreq
        DOUBLE PRECISION :: T, totMag, totEng

        ALLOCATE (mesh(meshSize, meshSize))

        CALL initialize()

        WRITE(*,*)
        CALL magnetizationCalc(totMag)
        WRITE(*,*) "  Initial Magnetization : ", totMag
        CALL totalEnergy(totEng)
        WRITE(*,*) "  Initial Energy : ", totEng
        WRITE(*,*)

        CALL eq_run_dump(T, dumpFreq)

        DEALLOCATE(mesh)

      END SUBROUTINE perEnsemble_dump

      SUBROUTINE eq_run_dump(T, dumpFreq)

        USE global_variables

        IMPLICIT NONE

        INTEGER :: step, i, j, dumpFreq
        DOUBLE PRECISION :: T, rand, totMag, totEng
        CHARACTER(len=256) :: fileName

        OPEN(3000, FILE = "magnetization.dat", STATUS = "NEW")
        OPEN(4000, FILE = "energy.dat", STATUS = "NEW")

        DO step = 1, equilibration_step
          IF (MOD(step, 1000000) .EQ. 0) THEN
            WRITE(*,*) "  Step : ", step
          END IF

          i = INT(rand() * meshSize) + 1
          j = INT(rand() * meshSize) + 1

          CALL swap(i, j, T)

          IF (MOD(step - 1, dumpFreq) .EQ. 0) THEN
            CALL magnetizationCalc(totMag)
            WRITE(3000, "(I,F)") step, totMag
            
            CALL totalEnergy(totEng)
            WRITE(4000, "(I,F)") step, totEng

            WRITE(fileName, "(I08)") step
            CALL saveMesh(fileName)
          END IF

        END DO

      END SUBROUTINE eq_run_dump

!     Main Program
      PROGRAM Monte_Carlo

      USE global_variables
      IMPLICIT NONE

      INTEGER :: i, j, step
      DOUBLE PRECISION :: rand
      CHARACTER(len=256) :: line
      DOUBLE PRECISION :: totMag, E, T, totEng
      DOUBLE PRECISION :: totMagAvg
      INTEGER :: dumpFreq

      ! Read input file
      OPEN(1000, FILE = "dump.in", STATUS = "OLD")

      READ(1000, "(A5, I)") line, seed
      READ(1000, "(A9, I)") line, meshSize
      READ(1000, "(A9, I)") line, coupling
      READ(1000, "(A2, I)") line, k
      READ(1000, "(A19, I)") line, equilibration_step
      READ(1000, "(A2, F5.2)") line, T
      READ(1000, "(A9, I)") line, dumpFreq

      CLOSE(1000)

      ! Check input file
      WRITE(*,*) "  < Check Your Input Here ! > "
      WRITE(*,*)
      WRITE(*,*) "  seed : ", seed
      WRITE(*,*) "  meshSize : ", meshSize
      WRITE(*,*) "  coupling : ", coupling
      WRITE(*,*) "  k : ", k
      WRITE(*,*) "  equilibration_step : ", equilibration_step
      WRITE(*,*) "  T : ", T
      WRITE(*,*) "  dumpFreq : ", dumpFreq
      WRITE(*,*)

      ! Seed Setting
      WRITE(*,*) "  [SinsuSquid] : Seed Setting ..."
      CALL SRAND(seed)
      WRITE(*,*) "  [SinsuSquid] : Seed Setting Complete"

      ! Start Simulation
      WRITE(*,*)
      WRITE(*,*) "  [SinsuSquid] : Starting Monte-Carlo Simulation ..."

      CALL perEnsemble_dump(T, dumpFreq)

      WRITE(*,*)
      WRITE(*,*) "  [SinsuSquid] : Monte-Carlo Simulation Complete"

      WRITE(*,*)
      WRITE(*,*) "  [SinsuSquid] : All Jobs are Done ! >:D"

      END PROGRAM Monte_Carlo

