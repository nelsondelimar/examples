!=====================================================================
      SUBROUTINE GSUM(xobs,yobs,zo,NPo,Xcp,Ycp,zt,zb,VdR,NPp,DXp,DYp,GC)
!   -----------------------------------------------------------------------------    
!   EVALUATION OF GRAVITY SIGNAL DUE TO ENTIRE PRISM MODEL FOR EACH OBSERVATION POINT 
!   Input parameters:
!    xobs,yobs,zo: Npo dimensional vectors of observation positions (km)
!    NPo: number of observations
!    Xcp,Ycp,zt,zb: NPb dimensional vectors of prism positions (Xcp,Ycp: center of prism, 
!                   zt: top of prism, zb: bottom of prism)	 (km)
!    VdR: NPb dimensional vectors of horizontal density distribution for the prism model (g/cm^3)
!    NPp: number of prisms
!    DXp,DYp: horizontal length of prisms	 (km)
!   comment: z is positive downwards. Values above zero level are negative.
!   Ouput Parameters:
!    gzT: NPo dimensional vector of gravity field in each observation point due to all prisms	 (mGal)
!   -----------------------------------------------------------------------------    
      IMPLICIT NONE
      INTEGER NPo,NPp,IO,IB,ID,KNC
      REAL(4) xobs(NPo),yobs(NPo),zo(NPo),GC(NPo)
      REAL(4) Xcp(NPp),Ycp(NPp),zt(NPp),zb(NPp),VdR(NPp)
      REAL(4) DXp,DYp,dR,dRsi,dgzp,sdgzp,x1,x2,y1,y2,ztc,zbc,dz,XO,YO
      REAL(4) GBOX 
      
      DO IO = 1, NPO
		sdgzp = 0.0
		XO = xobs(IO) !/1000 ! m TO km (SI)
		YO = yobs(IO) !/1000 ! m TO km (SI)
		DO IB = 1, NPp
			y1 = (Ycp(IB) - DYp/2) !/1000 ! m TO km (SI)
			y2 = (Ycp(IB) + DYp/2) !/1000 ! m TO km (SI)
			x1 = (Xcp(IB) - DXp/2) !/1000 ! m TO km (SI)
			x2 = (Xcp(IB) + DXp/2) !/1000 ! m TO km (SI)
			IF (ZB(IB).GT.0.0) THEN	    
			    dRsi = VdR(IB)
		        dgzp = GBOX(IO,IB,XO,YO,zo(IO),x1,y1,zt(IB),x2,y2,zb(IB),
	1	                dRsi)
	      ELSE
	          dgzp = 0.0
	      END IF
			sdgzp = sdgzp + dgzp
		END DO
		GC(IO) = sdgzp
!		WRITE(06,*) 'Observacao numero: ', IO
	END DO 
	
	RETURN
      END

!=====================================================================
	FUNCTION gbox(IO,IB,x0,y0,z0,x1,y1,z1,x2,y2,z2,rho)
!   ------------------------------------------------------------------
!	Function GBOX computes the vertical attraction of a
!	rectangular prism. Sides of prism are parallel to x,y,z axes,
!	and z axis is vertical down.
!
!	Input parameters:
!	Observation point is (xO,yO,zO). The prism extends from xl
!	to x2, from yl to y2, and from zl to z2 in the x, y, and z
!	directions, respectively. Density of prism is rho. All
!	distance parameters in units of km; rho in units of
!	kg/(m**3).
!
!	Output parameters:
!	Vertical attraction of gravity g, in mGal.
!   ------------------------------------------------------------------    
      IMPLICIT NONE
	real(4) gamma, twopi, si2mg, km2m
	real(4) x0,y0,z0,x1,y1,z1,x2,y2,z2,rho
	real(4) x(2),y(2),z(2)
	real(4) rijk,ijk,arg1,arg2,arg3,sum,gzd
	real(4) gbox

	integer isi(2),i,j,k,IO,IB

	isi(1) = -1
	isi(2) = 1
	gamma = 6.670e-11
	twopi = 6.2831853
      si2mg = 1.e5
      km2m = 1.e3

	x(1)=x0-x1
	y(1)=y0-y1
	z(1)=z0-z1
	x(2)=x0-x2
	y(2)=y0-y2
	z(2)=z0-z2
	
	sum=0.
	do i=1,2
		do j=1,2
			do k=1,2
				rijk=sqrt((x(i)**2)+(y(j)**2)+(z(k)**2))
				ijk=(isi(i))*(isi(j))*(isi(k))
				arg1=atan2((x(i)*y(j)),(z(k)*rijk))
				if(arg1.lt.0.)arg1=arg1+twopi
				arg2=rijk+y(j)
				arg3=rijk+x(i)
				IF(arg2.le.0)THEN 
				    WRITE(06,*)'GB0X: Bad field point'
				    WRITE(06,*)IO,IB,Y0,Y1,Y2,X0,X1,X2,Z0,Z1,Z2
                      pause
				    !STOP
				END IF
				IF(arg3.le.0)THEN
				    WRITE(06,*)'GBOX: Bad field point'
				    WRITE(06,*)IO,IB,Y0,Y1,Y2,X0,X1,X2,Z0,Z1,Z2
                      pause
				    !STOP
				END IF
				arg2=log(arg2)
				arg3=log(arg3)
				sum=sum+ijk*(z(k)*arg1-x(i)*arg2-y(j)*arg3)
	        end do
	    end do
	end do

	gzd = Rho*gamma*sum*si2mg*km2m
	gbox = gzd

	return
	end

!------------------------------------- gera grid saída surfer
	SUBROUTINE WR_FM_XYZ_V(X,Y,Z,NPTS,FILNAM)
!-------------------------------------------------------------
!   output a grid as three-column file
! Input:
!   X,Y,Z: NPTS dimensional vectors of position and values of grid-points
!   FILNAM: output file name
!-------------------------------------------------------------
      IMPLICIT NONE
 	INTEGER(4) NPTS,LFIL,I
	REAL(4) X(NPTS),Y(NPTS),Z(NPTS)
	CHARACTER*(*) FILNAM

105   FORMAT(1X,ES17.4,2X,ES17.4,2X,ES17.4)	
	
      LFIL = 155

	OPEN (UNIT=LFIL,STATUS='UNKNOWN',FILE=FILNAM,FORM='FORMATTED')   
	
	DO I = 1, NPTS
	    WRITE(LFIL,105)X(I),Y(I),Z(I)
	END DO

      CLOSE (UNIT=LFIL) 

	RETURN
	END

!--------------------------------------- gera arquivo para plot direto no surfer
	SUBROUTINE WR_FM_GRD_V(NPT,NPX,NPY,VECT,X0,XMAX,Y0,YMAX,FILNAM)
!--------------------------------------------------------------------
!   output a grid surfer file 
!   Input:
!   X,Y,Z: NPTS dimensional vectors of position and values of grid-points
!   X0,XMAX,Y0,YMAX: grid-corners 
!   NPX,NPY: horizontal grid dimension
!   FILNAM: output file name
!-------------------------------------------------------------
      IMPLICIT NONE 
      INTEGER(4) NPT,NPX,NPY,KV,I,J
      REAL(4) DADOS(NPY,NPX),VECT(NPT)
	REAL(4) X0,Y0,XMAX,YMAX,DAT_MAX,DAT_MIN,FLAG,FLAG_SURFER
      CHARACTER*(*) FILNAM
	INTEGER(4) LFIL

	DATA FLAG/-9999.00/
	DATA FLAG_SURFER/1.70141E+038/
	
	DAT_MAX = -1.e10
	DAT_MIN =  1.e10

	KV = 0
      DO I = 1,NPX
	    DO J = 1,NPY
			KV = KV + 1
			DADOS(I,J) = VECT(KV)
		END DO
	END DO

	DO I = 1, NPX
		DO J = 1, NPY
			IF(DADOS(I,J).NE.FLAG)THEN
				IF(DADOS(I,J).GT.DAT_MAX) DAT_MAX=DADOS(I,J)
				IF(DADOS(I,J).LT.DAT_MIN) DAT_MIN=DADOS(I,J)
			END IF  
		END DO
	END DO
!-------------------------ASCII Grid File Format
	
	LFIL = 144
	OPEN (UNIT=LFIL,STATUS='UNKNOWN',FILE = FILNAM,FORM='FORMATTED')   
		
	WRITE(LFIL,1)
	WRITE(LFIL,2)NPX,NPY
	WRITE(LFIL,3)X0,XMAX
	WRITE(LFIL,3)Y0,YMAX
	WRITE(LFIL,3)DAT_MIN,DAT_MAX
	
1	FORMAT(BN,'DSAA')
2     FORMAT(I,1X,I)
3     FORMAT(ES12.4,1X,ES12.4)
	
22	FORMAT($ES12.4)

		
	DO I=1,NPx
		DO J=1,NPy
            IF(DADOS(I,J).EQ.FLAG)DADOS(I,J)=FLAG_SURFER
		END DO
	  WRITE(LFIL,22) (DADOS(I,J),J=1,NPX)
		WRITE(LFIL,*) 
	END DO

	CLOSE (UNIT = LFIL)
	RETURN
	END
