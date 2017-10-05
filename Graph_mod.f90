!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
!A SET OF FUNCTIONS TO DRAW ELEMENTS AND SUPPORTS--------------------------------------------------------------------------------------------
!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM

MODULE DRAW

USE DFLIB
	TYPE (WXYCOORD) WXY								!ENABLES GRAPHICAL ACTIONS
	TYPE (QWINFO) A									!"A" IS DEFINED AS A WXYCOORD VARIABLE FOR THE "GRAPH" WINDOW
	!I=DISPLAYCURSOR ($GCURSORON)				"***	TO BE COPIED TO THE MAIN PROGRAMME   ***"
	!I=SETBKCOLOR (1)
	!I=SETCOLOR (4)
	!a.type=qwin$max
	!i=setwsizeqq(qwin$framewindow,a)
	!i=setwsizeqq(0,a)
	!i=setwindow(.true.,-5,-5,L,H)
	!call clearscreen($gclearscreen)


CONTAINS
!********************************************************************************************************************************************
FUNCTION LINE (X1,Y1,X2,Y2)						!DRAWS A LINE BETWEEN THE TWO GIVEN POINTS
	REAL*8 X1,Y1,X2,Y2							
	CALL MOVETO_W(X1,Y1,WXY)
	LINE=LINETO_W(X2,Y2)
END FUNCTION LINE

!********************************************************************************************************************************************
FUNCTION DLINE (X1,Y1,X2,Y2)						!DRAWS A DOTTED-LINE BETWEEN THE TWO GIVEN POINTS
	REAL*8 X1,Y1,X2,Y2	
							
	CALL MOVETO_W(X1,Y1,WXY)
	DLINE=LINETO_W(X2,Y2)
END FUNCTION DLINE
	
!********************************************************************************************************************************************
FUNCTION JOINT (X1,Y1,L,H)							!DRAWS A JOINT AT THE GIVEN CO-ORDINATES (CODE 1)
	REAL*8 X1,Y1,R,L,H
	R=H/100
	JOINT=ELLIPSE_W($GBORDER,X1-R,Y1+R,X1+R,Y1-R)
END FUNCTION JOINT

!********************************************************************************************************************************************
FUNCTION PIN (X1,Y1,L,H)							!DRAWS A FREE-TO-TURN PINNED SUPPORT AT THE GIVEN CO-ORDINATES (CODE 2)
	REAL*8 X1,Y1,DX,DY,L,H
	DX=2*H/100
	DY=4*L/100
	PIN=JOINT (X1,Y1,L,H)
	PIN=LINE(X1,Y1,X1-DX,Y1-DY)
	PIN=LINE(X1,Y1,X1+DX,Y1-DY)
	PIN=LINE(X1-DX,Y1-DY,X1+DX,Y1-DY)
END FUNCTION PIN

!********************************************************************************************************************************************
FUNCTION ROLLERH (X1,Y1,L,H)							!DRAWS A HORIZENTAL ROLLER SUPPORT AT THE GIVEN CO-ORDINATES (CODE 3-1)
	REAL*8 X1,Y1,X2,Y2,L,H
	DX=2*H/100
	DY=L/100
	ROLLERH=PIN (X1,Y1,L,H)
	ROLLERH=LINE (X1+2*DX,Y1-4*DY,X1-2*DX,Y1-4*DY)
END FUNCTION ROLLERH

!********************************************************************************************************************************************
FUNCTION ROLLERV (X1,Y1,L,H)							!DRAWS A VERTICAL ROLLER SUPPORT AT THE GIVEN CO-ORDINATES (CODE 3-2)
	REAL*8 X1,Y1,X2,Y2,L,H
	DX=4*H/100
	DY=2*L/100
	ROLLERV=JOINT (X1,Y1,L,H)
	ROLLERV=LINE(X1,Y1,X1-DX,Y1+DY)
	ROLLERV=LINE(X1,Y1,X1-DX,Y1-DY)
	ROLLERV=LINE(X1-DX,Y1-DY,X1-DX,Y1+DY)
	ROLLERV=LINE (X1-1.5*DX,Y1-DY,X1-1.5*DX,Y1+DY)
END FUNCTION ROLLERV

!********************************************************************************************************************************************
FUNCTION FIXED (X1,Y1,L,H)							!DRAWS A FIXED-END SUPPORT AT THE GIVEN CO-ORDINATES (CODE 4)
	REAL*8 X1,Y1,L,H
	DY=2*H/100
	DX1=1*L/400
	DX2=1*L/300
	DX3=1*L/200
	FIXED=LINE(X1,Y1+DY,X1,Y1-DY)
	FIXED=LINE(X1-DX1,Y1+DY,X1-DX1,Y1-DY)
	FIXED=LINE(X1-DX2,Y1+DY,X1-DX2,Y1-DY)
	FIXED=LINE(X1-DX3,Y1+DY,X1-DX3,Y1-DY)
END FUNCTION FIXED

!********************************************************************************************************************************************
FUNCTION GUIDEH (X1,Y1,L,H)							!DRAWS A HORIZENTAL GUIDED SUPPORT AT THE GIVEN CO-ORDINATES (CODE 5-1)
	REAL*8 X1,Y1,L,H
	DX=2*L/100
	DY=2*H/100
	GUIDEH=LINE (X1,Y1+DY,X1,Y1-DY)
	GUIDEH=ELLIPSE_W($GBORDER,X1-DX,Y1+DY,X1,Y1)
	GUIDEH=ELLIPSE_W($GBORDER,X1-DX,Y1,X1,Y1-DY)
END FUNCTION GUIDEH

!********************************************************************************************************************************************
FUNCTION GUIDEV (X1,Y1,L,H)							!DRAWS A VERTICAL GUIDED SUPPORT AT THE GIVEN CO-ORDINATES (CODE 5-2)
	REAL*8 X1,Y1,L,H
	DX=2*L/100
	DY=2*H/100
	GUIDEV=LINE (X1-DX,Y1,X1+DX,Y1)
	GUIDEV=ELLIPSE_W($GBORDER,X1-DX,Y1,X1,Y1-DY)
	GUIDEV=ELLIPSE_W($GBORDER,X1,Y1,X1+DX,Y1-DY)
END FUNCTION GUIDEV

!********************************************************************************************************************************************
FUNCTION GRID(L,H)									!DRAWS GRIDLINES
	REAL*8 X1,Y1,X2,Y2,L,H
	X1=-2; Y1=-1*H
	X2=-2; Y2=-1*H
	K=2*MAX(H,L)+1
	DO I=1,K
		DO J=1,K
			GRID=DLINE(X1,Y1,X1,Y2)
			Y2=Y2+1
		END DO
	X1=X1+1
	END DO
	X1=-2; Y1=-2; X2=-2
	DO I=1,K
		DO J=1,K
			GRID=DLINE(X1,Y1,X2,Y1)
			X2=X2+1
		END DO
	Y1=Y1+1
	END DO
END FUNCTION GRID
!********************************************************************************************************************************************	

END MODULE DRAW
!********************************************************************************************************************************************
