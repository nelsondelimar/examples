  PROGRAM CORRELATION
  !----------------------------------------------------------------------  
  ! THIS PROGRAM CALCULATES THE CORRELATION BETWEEN TWO N-DIMENTIONAL 
  ! VECTORS USING A SUBROUTINE OF THE REGULAR FORMULATION.
  ! IN LATEX:
  !
  ! c = \dfrac{
  ! \sum\limits_{i=1}^{n} 
  ! \left[(x_i - \bar{x})(y_i - \bar{y})
  ! \right]}{
  ! \sum\limits_{i=1}^{n} (x_i - \bar{x})^2 \cdot
  ! \sum\limits_{i=1}^{n} (y_i - \bar{y})^2}
  !
  ! PARAMETERS OF INPUT:
  ! * VEC1 - VECTOR - N DIMENTIONAL VECTOR
  ! * VEC2 - VECTOR - N DIMENTIONAL VECTOR
  ! * NSIZE - INTEGER - NUMBER OF POINTS OF VECTORS
  !
  ! PARAMETER OF OUTPUT:
  ! * COEF - REAL - PEARSON CORRELATION COEFFICIENT
  !
  ! NOTE: VALUE OF COEFFICIENT MUST BE BETWEEN +/- 1
  !
  !----------------------------------------------------------------------
  IMPLICIT NONE

  ! VARIABLES
  REAL(4), ALLOCATABLE :: VEC1(:), VEC2(:)
  REAL(4) :: COEF
  INTEGER :: I, J
  INTEGER :: N = 60501 ! NUMBER OF POINTS OF VECTORS

  OPEN (10,	FILE='input1.txt', ACTION ='read', STATUS ='old')
  OPEN (80,	FILE='input2.txt', ACTION ='read', STATUS ='old')

  ALLOCATE(VEC1(N))
  ALLOCATE(VEC2(N))

  ! READ EACH VALUE OF ARRAYS
  DO I = 1, N
    READ(10,*) (VEC1(I))
    READ(80,*) (VEC2(I))
  END DO

  ! CALLING THE SUBROUTINE
  CALL MY_CORRELATION(VEC1,VEC2,N,COEF)

  PRINT*, COEF
  STOP
  END PROGRAM CORRELATION