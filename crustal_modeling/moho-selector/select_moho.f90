	! Programa para selecionar um retangulo de dentro de um conjuntoi de dados em lat long 
	PROGRAM seleciona
	IMPLICIT NONE
	REAL(4), ALLOCATABLE:: Mal(:,:)
	REAL(4), ALLOCATABLE:: lat(:),long(:),Zcrust(:)
	REAL(4) latN, latS, longW, longE
	INTEGER NP,LD,LS,I,J,alloc_error
	CHARACTER(50)FileIN, FileOUT,ARQUIVO

    NP = 30351 ! número de linhas do arquivo
    latN = -1.
    latS = -20.
    longW = 295.
    longE = 325.
    
    ALLOCATE(Mal(NP,3),STAT = alloc_error)
    ALLOCATE(lat(NP),STAT = alloc_error)
	ALLOCATE(long(NP),STAT = alloc_error)
    ALLOCATE(Zcrust(NP),STAT = alloc_error)
    
    ! lendo o arquivo de dados
	LD = 11
    FileIN = "south_american_moho.txt"
	OPEN (UNIT = LD, FILE = FileIN)
	
	DO I = 1, NP
	    READ(LD,*)(MAl(I,J), J = 1, 3)
    END DO
    
	CLOSE (UNIT=LD)
	
	DO I = 1, NP
	  Lat(I) = Mal(I,1)
	  Long(I) = Mal(I,2)
	  Zcrust(I) = Mal(I,3) !Mal(I,3)!*-1.
	END DO
	
	LS = 22
    FileOUT = "moho_selected.txt"
	OPEN (UNIT = Ls, FILE = FileOUT)

	DO I = 1, NP
	    IF(latN>=Lat(I).and.latS<=Lat(I)) THEN
	        IF(longE>=Long(I).and.longW<=Long(I)) THEN
	            Write(LS,*)Lat(I),Long(I),Zcrust(I)
	        END IF
	    END IF
	END DO
	            
	deallocate(Mal,STAT = alloc_error)
    deallocate(Lat,STAT = alloc_error)
	deallocate(Long,STAT = alloc_error)
	deallocate(ZCrust,STAT = alloc_error)
	
	STOP
	END PROGRAM seleciona
            
	            

	

    
    


