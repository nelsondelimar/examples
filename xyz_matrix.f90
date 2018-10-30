PROGRAM XYZ_2_mat

! Program that converts a XYZ file to matrix data file

! Definition of all variables
REAL(4), ALLOCATABLE :: DAT(:,:), MAT(:,:)
REAL(4) :: X0,Y0,DY,DX,CONT,DUMMY
INTEGER :: ERRO_ALLOC, KT, NOY, NOX, NPTS, INPUTXYZ, OUTMATRIX, OUTVECTOR
	
! Choose the format of the write file
10    FORMAT(100000F14.6)	
90    FORMAT(2X,F10.2,2X,F10.2,2X,F10.6)
	
NOY = 613 ! Number of points in Y
NOX = 893 ! Number of points in X
NPTS = NOY*NOX ! Total number of points
           
! Allocate the need memory
ALLOCATE (DAT(NPTS,3),STAT=ERRO_ALLOC)
ALLOCATE (MAT(NOY,NOX),STAT=ERRO_ALLOC)

! Open the input unit file
INPUTXYZ = 11
OPEN(UNIT = INPUTXYZ, FILE='file_name.dat')

OUTMATRIX = 21
OPEN (UNIT = OUTMATRIX, STATUS = 'UNKNOWN', FILE = 'output_file_name.dat')

! Reading the input file
DO I = 1, NPTS
	READ(INPUTXYZ,*)(DAT(I,LN),LN = 1,3)
END DO
      
! Computes the value for the new matrix file
K = 0
DO I = 1, NOY
	DO J = 1, NOX
		K = K + 1
		MAT(I,J) = DAT(K,3)
	END DO
END DO
	
! Write the output file after the conversion
DO I = 1, NOY
	WRITE(OUTMATRIX,10)(MAT(I,J),J = 1, NOX)
END DO

! Deallocate the used memory for the data file
DEALLOCATE (DAT,STAT=ERRO_ALLOC)
DEALLOCATE (MAT,STAT=ERRO_ALLOC)

! Finish the program
STOP
END PROGRAM XYZ_2_mat
