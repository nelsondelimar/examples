  SUBROUTINE MY_CORRELATION(DATA1, DATA2, NPTS, COEF)
  IMPLICIT NONE
  !-------------------------------------------------------------------
  ! THIS SUBROUTINE CALCULATE THE CORRELATION BETWEEN TWO VECTORS OF
  ! SAME LENGTH.
  !-------------------------------------------------------------------
  INTEGER, INTENT(IN)   :: NPTS
  REAL(4), INTENT(IN)   :: DATA1(NPTS), DATA2(NPTS)
  REAL(4), INTENT(OUT)  :: COEF
  REAL(4)               :: NUM(NPTS), DEN1(NPTS), DEN2(NPTS), MEAN1, MEAN2
  INTEGER               :: I

  MEAN1 = SUM(DATA1)/NPTS
  MEAN2 = SUM(DATA2)/NPTS

  DO I = 1, NPTS
    NUM(I) = (DATA1(I) - MEAN1) * (DATA2(I) - MEAN2)
    DEN1(I) = (DATA1(I) - MEAN1)**2
    DEN2(I) = (DATA2(I) - MEAN2)**2
  END DO

  COEF = SUM(NUM)/((SUM(DEN1)*SUM(DEN2))**(0.5))

  RETURN
  END SUBROUTINE MY_CORRELATION