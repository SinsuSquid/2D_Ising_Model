      SUBROUTINE perEnsemble(T, totMagAvg)
!     Task requied for each ensemble

        USE global_variables

        IMPLICIT NONE

        DOUBLE PRECISION :: T, totMag, totMagAvg

        ALLOCATE(mesh(meshSize, meshSize))

        CALL initialize()

        CALL eq_run(T)

        CALL prod_run(T, totMag, totMagAvg)

        DEALLOCATE(mesh)

      END SUBROUTINE perEnsemble

      SUBROUTINE eq_run(T)
!     Task required for each equilibration run

        USE global_variables

        IMPLICIT NONE

        INTEGER :: step, i, j
        DOUBLE PRECISION :: T, rand

        DO step = 1, equilibration_step
          i = INT(rand() * meshSize) + 1
          j = INT(rand() * meshSize) + 1

          CALL swap(i, j, T)
        END DO

      END SUBROUTINE eq_run

      SUBROUTINE prod_run(T, totMag, totMagAvg)
!     Task requied for each production run

        USE global_variables

        IMPLICIT NONE

        INTEGER :: step, i, j
        DOUBLE PRECISION :: T, rand, totMag, totMagAvg

        totMagAvg = 0.d0

        DO step = 1, production_step
          i = INT(rand() * meshSize) + 1
          j = INT(rand() * meshSize) + 1

          CALL swap(i, j, T)
          CALL magnetizationCalc(totMag)
          totMagAvg = totMagAvg + totMag
        END DO

        totMagAvg = totMagAvg / DBLE(production_step)

      END SUBROUTINE prod_run
