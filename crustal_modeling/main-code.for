	! Programa para calcular a anomalia gravimétrica de uma camada discretizada em prismas
	PROGRAM GPRISMAS
	IMPLICIT NONE
	REAL(4), ALLOCATABLE:: MLo(:,:),MLD1(:,:),MLD2(:,:)
	REAL(4), ALLOCATABLE:: XCP1(:),YCP1(:),ZTP1(:),ZBP1(:),DENP1(:)
	REAL(4), ALLOCATABLE:: XCP2(:),YCP2(:),ZTP2(:),ZBP2(:),DENP2(:)
	REAL(4), ALLOCATABLE:: XO(:),YO(:),ZO(:),GC1(:),GC2(:),GCTOTAL(:)
	REAL(4) DXP1,DYP1,DXP2,DYP2,hmeio,x0o,Xomax,Y0o,Yomax
	INTEGER Lo,Lp1,Lp2,npxo,npyo
	INTEGER NPO,NPD1,NPD2,I,J,alloc_error
	CHARACTER(4)EXTD,EXTG 
	CHARACTER(50)xyzobs,xyztopocrosta,xyzbasecrosta,ARQUIVO
	CHARACTER(200)doveread,dovewrite
	CHARACTER(250)FILEIMPUT,FILENAME

      EXTD = '.dat'
      EXTG = '.grd'
      
	OPEN(unit=01,file='Header.dat')

!	Leitura de Parâmetros

      read(01,*) NPO
	read(01,*) NPD1,DXP1,DYP1
	read(01,*) NPD2,DXP2,DYP2
	read(01,*) doveread
      read(01,*) dovewrite
      read(01,*) xyzobs  !!! => z deve ser lido positivo
      read(01,*) xyztopocrosta
      read(01,*) xyzbasecrosta

      DXP1 = DXP1/1000
      DYP1 = DYP1/1000
      DXP2 = DXP2/1000
      DYP2 = DYP2/1000      

      ALLOCATE(MLo(NPo,3),STAT = alloc_error)
      ALLOCATE(MLD1(NPD1,3),STAT = alloc_error)
      ALLOCATE(MLD2(NPD2,3),STAT = alloc_error)
	ALLOCATE(XCP1(NPD1),STAT = alloc_error)
      ALLOCATE(YCP1(NPD1),STAT = alloc_error)
      ALLOCATE(ZTP1(NPD1),STAT = alloc_error)
      ALLOCATE(ZBP1(NPD1),STAT = alloc_error)
      ALLOCATE(DENP1(NPD1),STAT = alloc_error)
      ALLOCATE(XCP2(NPD2),STAT = alloc_error)
      ALLOCATE(YCP2(NPD2),STAT = alloc_error)
      ALLOCATE(ZTP2(NPD2),STAT = alloc_error)
      ALLOCATE(ZBP2(NPD2),STAT = alloc_error)
      ALLOCATE(DENP2(NPD2),STAT = alloc_error)
      ALLOCATE(XO(NPO),STAT = alloc_error)
      ALLOCATE(YO(NPO),STAT = alloc_error)
      ALLOCATE(ZO(NPO),STAT = alloc_error)
      ALLOCATE(GC1(NPO),STAT = alloc_error)
      ALLOCATE(GC2(NPO),STAT = alloc_error)
      ALLOCATE(GCTOTAL(NPO),STAT = alloc_error)
	
!     ------------------------ LEITURA DOS ARQUIVOS DE INPUT	
	WRITE(06,*) "INICIANDO A LEITURA DOS ARQUIVOS DE INPUT"
	
	! posição das Observações
	Lo = 10
      FILEIMPUT = TRIM(DOVEREAD)//xyzobs
	OPEN (UNIT = Lo, FILE = FILEIMPUT)
	
	DO I = 1, NPo
	    READ(Lo,*)(MLo(I,J), J = 1, 3)
      END DO
	CLOSE (UNIT=Lo)
	
	DO I = 1, NPo
	  XO(I) = MLo(I,1)/1000 ! m TO km (SI)
	  YO(I) = MLo(I,2)/1000 ! m TO km (SI)
	  ZO(I) = 1.000 !((MLo(I,3)*-1)/1000) ! m TO km (SI)  !!! => tornando negativo 
        ! if (ABS(ZO(I)).LE.0.001) ZO(I) = -0.001 
	END DO

	X0o = MINVAL(XO)
	Xomax = MAXVAL(XO)
	Y0o = MINVAL(YO)
	Yomax = MAXVAL(YO)
	npxo = ((Xomax - x0o)/DXP1) + 1 ! Muito cuidado com DXP1  
	npyo = ((Yomax - y0o)/DYP1) + 1 ! Muito cuidado com DYP1
      
      write (06,*) npxo,npyo
	
	! Topografia
	Lp1 = 11
      FILEIMPUT = TRIM(DOVEREAD)//xyztopocrosta
	OPEN (UNIT = Lp1, FILE = FILEIMPUT)
	
	DO I = 1, NPD1
	    READ(Lp1,*)(MLD1(I,J), J = 1, 3)
      END DO
	CLOSE (UNIT=Lp1)
	
	DO I = 1, NPD1
	  XCP1(I) = (MLD1(I,1)/1000) ! m TO km (SI)
	  YCP1(I) = (MLD1(I,2)/1000) ! m TO km (SI)
	  ZTP1(I) = ((MLD1(I,3)*-1 )/1000) ! m TO km (SI) !!! => tornando negativo
        ! if (ABS(ZTP1(I)).LE.0.001) ZTP1(I) = 0. 
	  DENP1(I) = 2673.
	END DO
	     
	! Base dos Prismas
	Lp2 = 12
	FILEIMPUT = TRIM(DOVEREAD)//xyzbasecrosta
	OPEN (UNIT = Lp2, FILE = FILEIMPUT)
	
	DO I = 1, NPD2
	    READ(Lp2,*)(MLD2(I,J), J = 1, 3)
      END DO
	CLOSE (UNIT=Lp2)
	
	DO I = 1, NPD2
	  XCP2(I) = MLD2(I,1)/1000 ! m TO km (SI)
	  YCP2(I) = MLD2(I,2)/1000 ! m TO km (SI)
	  ZBP2(I) = MLD2(I,3)/1000 ! m TO km (SI) 
	  DENP2(I) = 2673.
      END DO
      
      hmeio = MAXVAL(ZTP1)+((MINVAL(ZBP2)-MAXVAL(ZTP1))*0.5)

      DO I = 1, NPD1
	  ZBP1(I) = hmeio
      END DO
      
      DO I = 1, NPD2
	  ZTP2(I) = hmeio
      END DO     
      
     	WRITE(06,*) "PLANO COMUM EM Z =", hmeio
      
	WRITE(06,*) "TERMINADA A LEITURA DOS ARQUIVOS DE INPUT"
     
      WRITE(06,*) "CALCULANDO A ANOMALIA DA CAMADA"

!     Anomalia da CAMADA DE CIMA
      CALL GSUM (xo,yo,zo,NPo,Xcp1,Ycp1,ztp1,zbp1,denp1,NPD1,DXp1,DYp1,
     &              gc1)
     
!     Anomalia da CAMADA DE BAIXO
      CALL GSUM (xo,yo,zo,NPo,Xcp2,Ycp2,ztp2,zbp2,denp2,NPD2,DXp2,DYp2,
     &              gc2)

!     CONTRIBUICAO DAS CAMADAS
      DO I = 1, NPD1
        GCTOTAL(I) = GC1(I) + GC2(I)
      END DO

!      Escrevendo OUTPUT    
      WRITE(06,*) "ESCREVENDO OS ARQUIVOS DE OUTPUT"

      ARQUIVO = 'gz_out'
	FILENAME = TRIM(dovewrite)//TRIM(ARQUIVO)//EXTD
      CALL WR_FM_XYZ_V(Xo,Yo,GCTOTAL,NPD1,FILENAME)
      
      ARQUIVO = 'gz_out'
	FILENAME = TRIM(dovewrite)//TRIM(ARQUIVO)//EXTG
      CALL WR_FM_GRD_V(NPD1,npxo,npyo,GCTOTAL,x0o,Xomax,Y0o,Yomax,
     &                  FILENAME)

      deallocate(MLo,STAT = alloc_error)
      deallocate(MLD1,STAT = alloc_error)
      deallocate(MLD2,STAT = alloc_error)
	deallocate(XCP1,STAT = alloc_error)
	deallocate(YCP1,STAT = alloc_error)
	deallocate(ZTP1,STAT = alloc_error)
	deallocate(ZBP1,STAT = alloc_error)
	deallocate(DENP1,STAT = alloc_error)
	deallocate(XCP2,STAT = alloc_error)
	deallocate(YCP2,STAT = alloc_error)
	deallocate(ZTP2,STAT = alloc_error)
	deallocate(ZBP2,STAT = alloc_error)
	deallocate(DENP2,STAT = alloc_error)
	deallocate(XO,STAT = alloc_error)
	deallocate(YO,STAT = alloc_error)
	deallocate(ZO,STAT = alloc_error)
	deallocate(GC1,STAT = alloc_error)
	deallocate(GC2,STAT = alloc_error)
	deallocate(GCTOTAL,STAT = alloc_error)

      WRITE(06,*) "FIM"
      STOP
	END PROGRAM GPRISMAS