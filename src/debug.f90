      PROGRAM debug

      USE global_variables
      IMPLICIT NONE

      DOUBLE PRECISION :: eng, T, totMag, totEng

      seed = 1215
      meshSize = 5
      coupling = 1
      k = 1
      T = 2.27d0
      
      CALL srand(seed)

      ALLOCATE(mesh(meshSize, meshSize))

      WRITE(*,*) "  [SinsuSquid] Here is initially allocated mesh !"
      CALL initialize()
      CALL printMesh()
      WRITE(*,*)

      WRITE(*,*) "  [SinsuSquid] Energy energy for row 3, column 4"
      CALL hamiltonian(3, 4, eng)
      WRITE(*,*) "  [SinsuSquid] Calculated Energy is"
      WRITE(*,*) "    Your answer : ", eng
      WRITE(*,*) "    (ans : +2.0)"
      WRITE(*,*)

      WRITE(*,*) "  [SinsuSquid] Should we swap the spin of row 3, column 4 ?"
      CALL swap(3, 4, T)
      CALL printMesh()
      WRITE(*,*) "    (ans : yes)"
      WRITE(*,*)

      WRITE(*,*) "  [SinsuSquid] Total magnetization for current configuration is"
      CALL magnetizationCalc(totMag)
      WRITE(*,*) "    Your answer : ", totMag
      WRITE(*,*) "    (ans : 0.28)"
      WRITE(*,*)

      WRITE(*,*) "  [SinsuSquid] Total energy for current configuration is"
      CALL totalEnergy(totEng)
      WRITE(*,*) "    Your answer : ", totEng
      WRITE(*,*) "    (ans : -12.0)"
      WRITE(*,*)

      DEALLOCATE(mesh)

      END PROGRAM debug
