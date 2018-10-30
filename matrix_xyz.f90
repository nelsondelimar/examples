PROGRAM mat_2_XYX

! Program that converts a matrix data file to a XYZ file.

! Definition of all variables
REAL(4), ALLOCATABLE :: Z(:,:)
CHARACTER*64 OUT_XYZ 
DATA OUT_XYZ/'output_filename.dat'/

! Read the initial values on Header data file

MYUNIT = 11
OPEN (UNIT = MYUNIT, FILE='header-file.txt')
READ (MYUNIT,*) NX,NY,DX,DY,X0,Y0

! Allocate the need memory
ALLOCATE (Z(NX,NY))

! Read all parameters and save them

IN_UNIT = 12
OPEN (UNIT = IN_UNIT, FILE = 'input_filename.dat') 

! Computes the operation for a saving data
DO I = 1, NX
	READ(IN_UNIT,*)(Z(I,J),J = 1, NY)
END DO

! Open the output unit for saving the output file
OUT_UNIT = 21
OPEN (UNIT = OUT_UNIT, STATUS='UNKNOWN', FILE = OUT_XYZ )   
	
! Computes the operation for the writing output file
X = X0
DO I = 1, NX
	Y = Y0
	DO J = 1, NY
		WRITE(OUT_UNIT,90)Y,X,Z(NX-(I-1),J)
		Y = Y + DY
	END DO
	X = X + DX
END DO

! Choose the format as you wish
90 FORMAT(2X,F15.3,2X,F15.3,2X,F15.3)

! Deallocate the used memory for the data file
DEALLOCATE (Z)

! Finish the program
STOP
END PROGRAM mat_2_XYX

