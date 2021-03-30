	PROGRAM AIRY_THICKNESS
    IMPLICIT NONE
    REAL(4), ALLOCATABLE :: MTOPO(:,:), XO(:), YO(:), ZO(:), THICK(:)
    INTEGER ERRO_ALLOC,LABEL1
	INTEGER NPOINTS, I, J
	!--------------------------------------------------!
	NPOINTS = 2601
	!--------------------------------------------------!
	LABEL1 = 11
	OPEN (UNIT = LABEL1, FILE = 'positiva.dat')
	CLOSE (UNIT = LABEL1)
	!--------------------------------------------------!
	ALLOCATE (MTOPO(NPOINTS,3),STAT=ERRO_ALLOC)
	ALLOCATE (XO(NPOINTS),STAT=ERRO_ALLOC)
	ALLOCATE (YO(NPOINTS),STAT=ERRO_ALLOC)	
	ALLOCATE (ZO(NPOINTS),STAT=ERRO_ALLOC)
	ALLOCATE (THICK(NPOINTS),STAT=ERRO_ALLOC)

	DO I= 1,NPOINTS
	READ(LABEL1,*)(MTOPO(I,J), J = 1, 3)
    END DO
	CLOSE (UNIT=LABEL1)
	!--------------------------------------------------!
	DO I = 1, NPOINTS
    XO(I) = MTOPO(I,1)
    YO(I) = MTOPO(I,2) 
    ZO(I) = MTOPO(I,3)
    PRINT*, I, XO(I), YO(I), ZO(I)
    END DO
	!--------------------------------------------------!    
	CALL THICKNESS_AIRY(ZO,NPOINTS,THICK)
	!--------------------------------------------------!
	!CALL WRITE_XYZ(XO, YO, THICK, NPOINTS, 'airy_thickness.dat')
	!--------------------------------------------------!    
	DEALLOCATE (MTOPO,STAT=ERRO_ALLOC)
	DEALLOCATE (XO,STAT=ERRO_ALLOC)
	DEALLOCATE (YO,STAT=ERRO_ALLOC)
	DEALLOCATE (ZO,STAT=ERRO_ALLOC)
	DEALLOCATE (THICK,STAT=ERRO_ALLOC)
    STOP
	END PROGRAM AIRY_THICKNESS