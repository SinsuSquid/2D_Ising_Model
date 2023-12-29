!     Main Program
      PROGRAM Monte_Carlo

      USE global_variables
      IMPLICIT NONE

      INTEGER :: i, j, step, temp, ensemble
      DOUBLE PRECISION :: rand
      CHARACTER(len=256) :: line, fileName
      DOUBLE PRECISION :: totMag, E, T, totEng
      DOUBLE PRECISION :: totMagAvg, ensembleAvg

      ! Read input file
      OPEN(1000, FILE = "parameters.in", STATUS = "OLD")

      READ(1000, "(A5, I)") line, seed
      READ(1000, "(A9, I)") line, meshSize
      READ(1000, "(A9, I)") line, coupling
      READ(1000, "(A2, I)") line, k
      READ(1000, "(A19, I)") line, equilibration_step
      READ(1000, "(A16, I)") line, production_step
      READ(1000, "(A12, I)") line, numEnsemble
      READ(1000, "(A9, I)") line, linspace

      CLOSE(1000)

      ! Check input file
      WRITE(*,*) "  < Check Your Input Here ! > "
      WRITE(*,*)
      WRITE(*,*) "  seed : ", seed
      WRITE(*,*) "  meshSize : ", meshSize
      WRITE(*,*) "  coupling : ", coupling
      WRITE(*,*) "  k : ", k
      WRITE(*,*) "  equilibration_step : ", equilibration_step
      WRITE(*,*) "  production_step : ", production_step
      WRITE(*,*) "  numEnsemble : ", numEnsemble
      WRITE(*,*)

      ! Seed Setting
      WRITE(*,*) "  [SinsuSquid] : Seed Setting ..."
      CALL SRAND(seed)
      WRITE(*,*) "  [SinsuSquid] : Seed Setting Complete"

!     Make Subroutines Listed Below !!
!
!     CALL magnetizationCalc(totMag)
!       > calculate total magnetization of a mesh
!       > returns total magnejization in double precision "totmag"
!     CALL hamiltonian(i, j, E)
!       > calculate energy for local configuration
!       > returns local energy in double precision "E"
!     CALL totalEnergy(totEng)
!       > calculate total energy for current configuration
!       > returns total energy in double precision "totEng"
!     CALL swap(i, j)
!       > compare energy between before & after swapping
!       > consider if swapping happens or not depending on Boltzmann dist.
!       > if it happens, swap the magnetization

      WRITE(*,*)
      WRITE(*,*) "  [SinsuSquid] : Starting Monte-Carlo Simulation ..."

      OPEN(2000, FILE = './plot.dat', STATUS = "NEW")

      DO temp = 1, linspace
        IF (MOD(temp, 10) .EQ. 0) THEN
          WRITE(*,*) "  [SinsuSquid] : Now processing t = ", temp, " ..."
        END IF

        T = DBLE(temp) * (4.d0 - 1.d0) / DBLE(linspace) + 1.d0

        ensembleAvg = 0.d0
        DO ensemble = 1, numEnsemble
          CALL perEnsemble(T, totMagAvg)
          ensembleAvg = ensembleAvg + DABS(totMagAvg)
        END DO
        ensembleAvg = ensembleAvg / DBLE(numEnsemble)
        WRITE(2000, "(F,F)") T, ensembleAvg
      END DO

      WRITE(*,*) "  [SinsuSquid] : Monte-Carlo Simulation Complete"

      CLOSE(2000)

      WRITE(*,*)
      WRITE(*,*) "  [SinsuSquid] : All Jobs are Done ! >:D"

      END PROGRAM Monte_Carlo
