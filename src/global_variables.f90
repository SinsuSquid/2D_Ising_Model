!     Module for storing variables that are used globally.
      MODULE global_variables

      IMPLICIT NONE

      INTEGER :: meshSize
      INTEGER :: seed
      INTEGER :: coupling
      INTEGER :: k
      INTEGER :: equilibration_step
      INTEGER :: production_step
      INTEGER :: numEnsemble
      INTEGER :: linspace
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: mesh(:,:)

      END MODULE global_variables
