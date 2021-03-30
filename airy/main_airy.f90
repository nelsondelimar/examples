	PROGRAM AIRY_THICKNESS

    IMPLICIT NONE

	REAL(4), ALLOCATABLE :: MTOPO(:,:), XO(:), YO(:), ZO(:), THICK(:)
    INTEGER ERRO_ALLOC,LABEL1,LABEL2
	INTEGER N, I, J

	!--------------------------------------------------!
	LABEL1 = 11
	OPEN (UNIT = LABEL1, FILE = 'input.dat',action='read',status='old')
	!--------------------------------------------------!
	
	N = 2601

	ALLOCATE (MTOPO(N,3),STAT=ERRO_ALLOC)
	ALLOCATE (XO(N),STAT=ERRO_ALLOC)
	ALLOCATE (YO(N),STAT=ERRO_ALLOC)
	ALLOCATE (ZO(N),STAT=ERRO_ALLOC)
	ALLOCATE (THICK(N),STAT=ERRO_ALLOC)

	DO I= 1,N
	READ(LABEL1,*)(MTOPO(I,J), J = 1, 3)
    END DO

	!--------------------------------------------------!
	DO I = 1, N
    XO(I) = MTOPO(I,1)
    YO(I) = MTOPO(I,2)
    ZO(I) = MTOPO(I,3)
    END DO
    print*, sum(zo)
    print*, sum(zo)/N
	!--------------------------------------------------!
	CALL THICKNESS_AIRY(ZO,N,THICK)
	!--------------------------------------------------!
	CALL WRITE_XYZ(XO, YO, THICK,N,'thickness.dat')
	!--------------------------------------------------!
	DEALLOCATE (MTOPO,STAT=ERRO_ALLOC)
	DEALLOCATE (XO,STAT=ERRO_ALLOC)
	DEALLOCATE (YO,STAT=ERRO_ALLOC)
	DEALLOCATE (ZO,STAT=ERRO_ALLOC)
	DEALLOCATE (THICK,STAT=ERRO_ALLOC)
    STOP
	END PROGRAM AIRY_THICKNESS
