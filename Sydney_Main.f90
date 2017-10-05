
!********************************************************************************************************************************************
!	!PROGRAMME TO ANALYSE 2-D FRAMES CONSIDERING THE LOADS AND ALL THE OTHER EXTERNAL EFFECTS.
!	!BY FARBOUD KHATAMI AND EHSAN TORKAMAN
!	!YEAR 1392 / 2013
!********************************************************************************************************************************************

	PROGRAM FRAME_ANALYSIS							!MAIN PROGRAMME
	
	USE DRAW										!GRAPHIC FUNCTIONS AND DRAWING TOOLS

!********************************************************************************************************************************************
!VARIABLE DECLERATIONS-----------------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************
	IMPLICIT REAL*8 (A-Z)
	INTEGER I,J,XX,YY,II,QQ	,NUMBER					!COUNTERS
	REAL*8 LENGHT									!LENGHT OF THE STRUCTURE
	REAL*8 HEIGHT									!HEIGHT OF THE STRUCTURE
	INTEGER*4 I4
	INTEGER NUM_NODE								!THE NUMBER OF NODES
	INTEGER NUM_ELEM								!THE NUMBER OF ELEMENTS
	INTEGER NUM_SUP									!THE NUMBER OF SUPPORTED NODES
	INTEGER NUM_TEMP								!THE NUMBER OF ELEMENTS WITH TEMPERATURE EFFECT
	INTEGER NUM_DEF									!THE NUMBER OF SUPPORTS WITH A DEFLECTION
	INTEGER SUPPORT									!DEFINES THE TYPE OF SUPPORT
	INTEGER NUM_LOAD								!THE NUMBER OF LOADED ELEMENTS
	INTEGER LD										!DEFINES THE TYPE OF LOAD ON THE MEMBERS
	INTEGER COUNT									!THE NUMBER OF FREE DEGREES OF FREEDOM
	INTEGER COUNTF									!FREE DEGREES OF FREEDOM COUNTER
	INTEGER COUNTS									!SECURED DEGREES OF FREEDOM COUNTER
	REAL*8 :: AREA, E, MI							!A, E, AND I OF A MEMBER
	REAL*8 , ALLOCATABLE, DIMENSION(:,:)::LOCAL		!JUST A DUMMY MATRIX TO BE USED IN TRANSFERING THE DATA FROM LOCAL TO GLOBAL CO-ORDINATE SYSTEM
	REAL*8 , ALLOCATABLE, DIMENSION(:)::  LOAD		!X_LOAD, Y_LOAD, AND MOMENT OF EACH NODE RESPECTIVELY 
	REAL*8 , ALLOCATABLE, DIMENSION(:)::  FORCE		!X_FORCE, Y_FORCE, AND MOMENT OF EACH NODE RESPECTIVELY ==> THE ANSWER
	REAL*8 , ALLOCATABLE, DIMENSION(:)::  FEM		!FIXED END X_FORCE,Y_FORCE AND MOMENT
	REAL*8 , ALLOCATABLE, DIMENSION(:)::  F_EL	     !NEWWWWWWWWWW
	REAL*8 , ALLOCATABLE, DIMENSION(:,:)::FEM_EL     !NEWWWWWWWWWW
	REAL*8 , ALLOCATABLE, DIMENSION(:)::  SCU        !NEWWWWWWWWWW
	REAL*8 , ALLOCATABLE, DIMENSION(:)::  DELTA_EL   !NWWWWWWWWWWW
	REAL*8 , ALLOCATABLE, DIMENSION(:)::  FEMTEMP		!A TEMPORARILY FEM
	REAL*8 , ALLOCATABLE, DIMENSION(:)::  FEMBACKUP	!A BACKUP OF FEM FOR A RAINY DAY!
	REAL*8 , ALLOCATABLE, DIMENSION(:)::  DISP		!X_DISPLACEMENT, Y_DISPLACEMENT, AND ROTATION OF EACH NODE RESPECTIVELY 
	REAL*8 , ALLOCATABLE, DIMENSION(:)::  DISPBACKUP !A BACKUP OF DISP
	REAL*8 , ALLOCATABLE, DIMENSION(:)::  DOF		!NODES DEGREE OF FREEDOM IN THE X, Y, AND MOMENT DIRECTION RESPECTIVELY
	REAL*8 , ALLOCATABLE, DIMENSION(:)::  DOFBACKUP	!A BACKUP OF DOF
	REAL*8 , ALLOCATABLE, DIMENSION(:,:)::NODE		!NODE CO-ORDINATES (X,Y)
	REAL*8 , ALLOCATABLE, DIMENSION(:,:)::NODENUM	!ELEMENT11 & ELEMENT12
	REAL*8 , ALLOCATABLE, DIMENSION(:,:)::ELEMENT	!ELEMENT CO-ORDINATES AND DATA (ELEMENT_NUMBER-X1-Y1-X2-Y2-L-THETHA(RAD)-E-I-A-NODE1-NODE2-NODE1X-NODE1Y-NODE2X-NODE2Y)
	REAL*8 , ALLOCATABLE, DIMENSION(:,:)::ELEMENT_BACKUP !NEWWWWWWWWWWWWWWWW
	REAL*8 , ALLOCATABLE, DIMENSION(:,:)::KL		!LOCAL STIFFNES MATRIX OF AN ELEMENT
	REAL*8 , ALLOCATABLE, DIMENSION(:,:)::KG		!GLOBAL STIFFNES MATRIX OF AN ELEMENT
	REAL*8 , ALLOCATABLE, DIMENSION(:,:)::K			!GLOBAL STIFFNES MATRIX OF THE STRUCTURE
	REAL*8 , ALLOCATABLE, DIMENSION(:,:)::T			!TRANSPOSITION MATRIX
	REAL*8 , ALLOCATABLE, DIMENSION(:,:)::KF		!FREE STIFFNESS MATRIX
	REAL*8 , ALLOCATABLE, DIMENSION(:,:)::KFINV		!INVERSE OF FREE STIFFNESS MATRIX
	REAL*8 , ALLOCATABLE, DIMENSION(:)::  FEMF		!FREE FEM
	REAL*8 , ALLOCATABLE, DIMENSION(:,:)::KFS		!FREE-SECURED SFIFFNESS MATRIX
	REAL*8 , ALLOCATABLE, DIMENSION(:)  ::DELTAF	!DELTA F
	REAL*8 , ALLOCATABLE, DIMENSION(:)  ::DELTAS	!DELTA S
	REAL*8 , ALLOCATABLE, DIMENSION(:,:)::S_DEF		!A SET OF DEFLECTED SUPPORTS, THEIR DOF NUMBER AND THEIR MAGNITUDE
	CHARACTER (LEN=1)::VERIFY						!Y OR N 
	CHARACTER (LEN=1)::STRTYPE                      !DETERMINES WEATHER THE STRUCTURE IS A TRUSS OR A FRAME	
	CHARACTER (LEN=1):: RESULT					
	TYPE (QWINFO) B									!"A" IS DEFINED AS A WXYCOORD VARIABLE FOR THE "DATA" WINDOW
	TYPE (QWINFO) C									!"A" IS DEFINED AS A WXYCOORD VARIABLE FOR THE "FRAME" WINDOW
	
	C.TYPE=QWIN$MAX						!MAXIMIZE
	I=SETWSIZEQQ(QWIN$FRAMEWINDOW,C)	!MAXIMIZES THE MAIN WINDOW

!********************************************************************************************************************************************
!SCREEN SETUP--------------------------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************	

	I=ABOUTBOXQQ ("Sydney STR."//CHAR(10)// "A 2-D frame and truss analysis programme."//CHAR(10)// "By Farboud Khatami and Ehsan Torkamane."C)	!ABOUT BOX CONTENTS

!DATA SCREEN SET UP
	OPEN (1,FILE="USER",TITLE="DATA")		
	I=SETBKCOLOR (3)								!SETS BACKGROUND COLOR TO "TURQUOIS"	
	call clearscreen($gclearscreen)							
	B.TYPE=QWIN$MAX									!MAXIMIZE
	I=SETWSIZEQQ(1,B)								!SETS SCREEN SIZE

!LOGO
	WRITE (1,113)
	WRITE (1,111), "__________________________________________________________________________________________________"
	WRITE (1,111), "      __     _     _    _____     _     _    _____    _     _            __    ______    ____     "
	WRITE (1,111), "    /    )   |    /     /    )    /|   /     /    '   |    /           /    )    /       /    )   "
	WRITE (1,111), "----\--------|---/-----/----/----/-| -/-----/__-------|---/------------\--------/-------/___ /----"
	WRITE (1,111), "     \       |  /     /    /    /  | /     /          |  /              \      /       /    |     "
	WRITE (1,111), "_(____/______|_/_____/____/____/___|/_____/____ ______|_/___________(____/____/_______/_____|___o_"
	WRITE (1,111), "              /                                        /                                          "
	WRITE (1,111), "          (_ /                                     (_ /                                           "
	WRITE (1,113)
	WRITE (1,112), "PRESS RETURN"
	VERIFY= GETCHARQQ ( ) 
	call clearscreen($gclearscreen)

!CREDITS							
	WRITE (1,103) "WELCOME TO THE 2-D FRAME ANALYSIS PROGRAMME"
	WRITE (1,101) "-------------------------------------------"
	WRITE (1,*)
	WRITE (1,108) "THIS IS A PROGRAMME TO CALCULATE THE REACTIONS AND THE MEMBER FORCES OF ANY SUPPOSED TWO-DIMENSIONAL FRAMES AND/OR TRUSSES BY THE MEANS OF STIFFNESS MATRIX ANALYSIS."
	WRITE (1,108) "THE PROGRAMME IS MADE AS A PART OF STRUCTURAL ANALYSIS II PROJECT, USING THE COMPAQ ", 425," VISUAL FORTRAN COMPILER BY 'FARBOUD KHATAMI' AND 'EHSAN TORKAMAN'."
	WRITE (1,*); WRITE (1,*)
	WRITE (1,108) "SPECIAL THANKS TO THE FOLLOWING PEOPLE, FOR WITHOUT WHOSE AID THE COMPLETION OF THIS PROGRAMME COULD NOT POSSIBLY OCCUR:"
	WRITE (1,101) "------------------------------------------------------------------------------------------------------------------------"
	WRITE (1,109) "M. SHIRAVAND", "S. MOUSAVI NADOUSHANI", "S. KHATAMI", "S. SOLEYMANI"
	WRITE (1,101) "NOTES"
	WRITE (1,101) "-----"
	WRITE (1,101) "NOTE 1: MAKE SURE YOU HAVE READ THE USER MANUAL CAREFULLY BEFORE USING THE PROGRAMME IN ORDER TO AVOID ANY KIND OF MISTAKE OR MISUNDERSTANDING."
	WRITE (1,101) "NOTE 2: TO USE THE PROGRAMME JUST SIMPLY FOLLOW THE INSTRUCTIONS PROVIDED IN THE TURQUOIS SCREENS AND THE REST IS UP TO THE PROGRAMME!"
	WRITE (1,101) "NOTE 3: WHEN YOU ARE ASKED FOR ANY KIND OF DATA CONCERNING THE ELEMENTS, KEEP IN MIND TO ENTER THE DATA DUE TO THE ELEMENT'S LOCAL CO-ORDINATE SYSTEM."
	WRITE (1,101) "NOTE 4: SHOULD YOU MAKE ANY MISTAKE IN INPUTTING THE DATA, YOU NEEDN'T WORRY. THE PROGRAMME WILL VERIFY ALL THE INFORMATION LATER ON."
	WRITE (1,101) "NOTE 5: NON-LETHAL USER MISTAKES ARE CORRECTED, BUT LETAL USER MISTAKES ARE SET TO TERTERMINATE THE PROGRAMME."
	WRITE (1,*)
	WRITE (1,101) "DISCLAIMER"
	WRITE (1,101) "----------"
	WRITE (1,114) 'SYDNEY OPERA HOUSE ICON TAKEN FROM "International Karate"',425,' (1985) BY "System 3"',425
	WRITE (1,114) '"That',96,'s All Folks" IS A "Warner Brothers"',425," QUOTE."
	WRITE (1,110) 425, " 1392-93/2013  GNU AFFERO GENERAL PUBLIC LICENSE V3."
	WRITE (1,103) "PRESS ANY KEY TO PROCEED"
	VERIFY= GETCHARQQ ( ) 
	CALL CLEARSCREEN($GCLEARSCREEN)
	
	!READING STRUCTURE SIZE SO AS TO DEFINE GRAPHIC CO-ORDINATES
	I=DISPLAYCURSOR ($GCURSORON)					!DISPLAYS CURSOR				
	WRITE (1,102) "GRAPHIC SCREEN SET UP","---------------------"
	WRITE (1,103) "IN ORDER TO HAVE A BETTER EXPERIENCE WITH THIS PROGRAMME, PLEASE ENTER THE MAXIMUM DIMENSIONS OF THE STRUCTURE:"
	WRITE (1,104) "THE HEIGHT OF THE STRUCTURE (m): "
	READ (1,*) HEIGHT
	WRITE (1,100) "THE LENTH OF THE STRUCTURE  (m): "
	READ (1,*) LENGHT
	WRITE (1,103) "PRESS ANY KEY TO PROCEED"
	VERIFY= GETCHARQQ ( ) 
	call clearscreen($gclearscreen)

	OPEN (2,FILE="USER",TITLE="GRAPHIC")
	I=SETBKCOLOR (0)								!SETS BACKGROUND COLOR TO "BLACK"
	CALL CLEARSCREEN ($GCLEARSCREEN)
	H=HEIGHT
	L=LENGHT
	I=SETWINDOW(.TRUE.,-1,-0.25*H,2*(L+1),H+0.5)		!***SETS THE CO-ORDINATE SYSTEM
	call clearscreen($gclearscreen)
	CALL SETLINESTYLE(#F0F0)						!SET LINE STYLE TO DOTTED LINE
	I=SETCOLOR (8)									!SETS GRIDLINE COLOR TO "GREY"
	G=GRID(2*(L+1),2*(H+1))							!DRAWS GRIDLINES
	CALL SETLINESTYLE(#FFFF)						!SET LINE STYLE TO SIMPLE LINE
	I=SETCOLOR (4)									!SETS GRAPH COLOR TO "RED"

! *** The following note applies to both child windows. ***
! Note that if the other window has focus, mouse events other than left
! button clicks will not be perceived by this window unless focus is given
! to it.  The left button is the exception since it always gives focus to
! the window on which it is clicked.

! Maximize the application's frame window; tile the Data and
! Graphic windows
	J=FOCUSQQ(1)
	A.type = QWIN$MAX
	i4 = setwsizeqq(QWIN$FRAMEWINDOW, A)
	i4 = clickmenuqq(QWIN$TILE)

!********************************************************************************************************************************************
!Structure Type (Frame / Truss)-------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************
500 WRITE (1,102) "STRUCTURE TYPE","--------------"
	WRITE (1,104) "ENTER THE TYPE OF STRUCTURE: 2-D TRUSS (T) OR 2-D FRAME (F)   "
	STRTYPE= GETCHARQQ ( ) 
	IF(STRTYPE=="t") STRTYPE="T"
	IF(STRTYPE=="f") STRTYPE="F"
	call clearscreen($gclearscreen)
SELECT CASE(STRTYPE)
!FRAME---------------------------------------------------------------------------------------------------------------------------------------
    CASE("F")
!********************************************************************************************************************************************
!NODES---------------------------------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************

2	    WRITE (1,102) "NODES","-----"
	    WRITE (1,104) "ENTER THE NUMBER OF NODES: "
	    READ (1,*),NUM_NODE								!TOTAL NUMBER OF NODES
	    ALLOCATE (NODE(NUM_NODE,2),DELTA_EL(6),F_EL(6),SCU(6),LOAD(3*NUM_NODE),FORCE(3*NUM_NODE),DISP(3*NUM_NODE),DOF(3*NUM_NODE),DOFBACKUP(3*NUM_NODE),FEM(3*NUM_NODE),FEMTEMP(3*NUM_NODE),FEMBACKUP(3*NUM_NODE),DISPBACKUP(3*NUM_NODE))
	    ALLOCATE (KL(6,6),KG(6,6),K(3*NUM_NODE,3*NUM_NODE),T(6,6),LOCAL(6,1))
	    K=0
	    KL=0
	    KG=0
	    T=0
	    LOCAL=0
	    NODE=0; LOAD=0; FORCE=0; DISP=0; DOFBACKUP=0; FEM=0; FEMTEMP=0; FEMBACKUP=0; DISPBACKUP=0
	    DOF=1								!SO THAT THE DEGREES OF FREEDOM NOT BE ZERO BY DEFAULT
	    WRITE (1,103) "NOW YOU WILL BE ASKED FOR NODE CO-ORDINATES. PLEASE MAKE SURE ALL CO-ORDINATES"
	    WRITE (1,101) "ARE POSITIVE NUMBERS."
	    DO I=1,NUM_NODE
		    WRITE (1,103) "NODE NUMBER", I,":"
		    WRITE (1,100) "X CO-ORDINATE: (m) "
		    READ (1,*) NODE(I,1)
		    WRITE (1,100) "Y CO-ORDINATE: (m) "
		    READ (1,*) NODE(I,2)
		    J=FOCUSQQ(2)								!SELECTS WINDOW NUMBER 2 (GRAPHICS)
		    P=SETPIXEL_W(NODE(I,1),NODE(I,2))			!DRAWS THE NODE		
		    J=FOCUSQQ(1)								!BACK TO SCREEN NUMBER 1 (DATA)
	    END DO
15	    WRITE (1,104) "DO YOU VERIFY ALL THE DISPLAYED NODE CO-ORDINATES? <Y/N> : "
	    VERIFY= GETCHARQQ ( ) 
	    IF (VERIFY=="Y".OR.VERIFY=="y") THEN
		    GOTO 13
	    ELSE IF (VERIFY=="N".OR.VERIFY=="n") THEN
		    call clearscreen($gclearscreen)
		    DEALLOCATE (NODE,DELTA_EL,F_EL,SCU,LOAD,FORCE,DISP,DOF,DOFBACKUP,FEM,FEMTEMP,FEMBACKUP,DISPBACKUP)
		    DEALLOCATE (KL,KG,K,T,LOCAL)
		    GOTO 2
	    ELSE
		    GOTO 15
	    END IF
13	    call clearscreen($gclearscreen)

!********************************************************************************************************************************************
!ELEMENTS------------------------------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************

11	    WRITE (1,102) "ELEMENTS", "--------"
	    WRITE (1,104) "ENTER THE NUMBER OF ELEMENTS: "
	    READ (1,*) NUM_ELEM																		!TOTAL NUMBER OF ELEMENTS
	    ALLOCATE (ELEMENT(NUM_ELEM,18),ELEMENT_BACKUP(NUM_ELEM,18),FEM_EL(6,NUM_ELEM))
	    ELEMENT=0
	    DO I=1,NUM_ELEM
		    ELEMENT(I,1)=I																		!THE NUMBER OF ELEMENT
		    WRITE (1,105)"ELEMENT NUMBER",I,":"
		    WRITE (1,100)"THE NUMBER OF NODE AT THE FIRST POINT OF MEMBER : "
		    READ (1,*) ELEMENT (I,11)															!NODE1
		    WRITE (1,100)"THE NUMBER OF NODE AT THE SECOND POINT OF MEMBER: "
		    READ (1,*) ELEMENT (I,12)															!NODE2
		    ELEMENT(I,2)=NODE(ELEMENT (I,11),1)													!X1
		    ELEMENT(I,3)=NODE(ELEMENT (I,11),2)													!Y1	
		    ELEMENT(I,4)=NODE(ELEMENT (I,12),1)													!X2
		    ELEMENT(I,5)=NODE(ELEMENT (I,12),2)													!Y2
		    ELEMENT(I,6)=SQRT((ELEMENT(I,3)-ELEMENT(I,5))**2+(ELEMENT(I,2)-ELEMENT(I,4))**2)	!LENGHT OF MEMBER																			
			ELEMENT(I,7)=ATAN((ELEMENT(I,5)-ELEMENT(I,3))/(ELEMENT(I,4)-ELEMENT(I,2)))	        !THETA (RAD)																	
		    ELEMENT (I,13)=NODE(ELEMENT (I,11),1)												!NODE 1 X
		    ELEMENT (I,14)=NODE(ELEMENT (I,11),2)												!NODE 1 Y
		    ELEMENT (I,15)=NODE(ELEMENT (I,12),1)												!NODE 2 X
		    ELEMENT (I,16)=NODE(ELEMENT (I,12),2)												!NODE 2 Y
		    ELEMENT (I,17)=(ELEMENT(I,4)-ELEMENT(I,2))/ELEMENT(I,6)										!COS THETHA		
		    ELEMENT (I,18)=(ELEMENT(I,5)-ELEMENT(I,3))/ELEMENT(I,6)										!SIN THETHA										
			    														!DRAWING THE ELEMENTS
		    J=FOCUSQQ(2)												!SELECTS WINDOW NUMBER 2 (GRAPHICS)
		    L=LINE(ELEMENT(I,2),ELEMENT(I,3),ELEMENT(I,4),ELEMENT(I,5))
		    J=FOCUSQQ(1)												!BACK TO SCREEN NUMBER 1 (DATA)
	    END DO
	    WRITE (1,103) "THE ELEMENTS OF THE STRUCTURE ARE DRAWN IN THE WINDOW TO THE RIGHT."
12	    WRITE (1,100) "DO YOU VERIFY ALL THE DISPLAYED MEMBER INFORMATION? <Y/N> : "
	    VERIFY= GETCHARQQ ( ) 
	    IF (VERIFY=="Y".OR.VERIFY=="y") THEN
		    GOTO 10
	    ELSE IF (VERIFY=="N".OR.VERIFY=="n") THEN
		    call clearscreen($gclearscreen)
		    J=FOCUSQQ(2)								!SELECTS WINDOW NUMBER 2 (GRAPHICS)
		    call clearscreen($gclearscreen)
		    L=LENGHT
		    H=HEIGHT
		    CALL SETLINESTYLE(#F0F0)						!SET LINE STYLE TO DOTTED LINE
		    I=SETCOLOR (8)									!SETS GRIDLINE COLOR TO "GREY"
		    G=GRID(2*(L+1),2*(H+1))							!DRAWS GRIDLINES
		    CALL SETLINESTYLE(#FFFF)						!SET LINE STYLE TO SIMPLE LINE
		    I=SETCOLOR (4)					
		    J=FOCUSQQ(1)								!BACK TO SCREEN NUMBER 1 (DATA)
		    DEALLOCATE(ELEMENT)
		    GOTO 11
	    ELSE
		    GOTO 12
	    END IF
10	    call clearscreen($gclearscreen)

!********************************************************************************************************************************************
!ELEMENTS AREA-------------------------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************

65	    WRITE (1,102) "SECTION PROPERTIES", "------------------", "CROSS-SECTION AREA"				!SECTION PROPERTIES
5	    NUM=NUM_ELEM
	    DO I=1,NUM
		    WRITE (1,103) "ELEMENT NUMBER", I,":"
		    NUMBER=I
		    WRITE (1,100) "CROSS-SECTION AREA? (mm^2) "
		    READ (1,*) AREA
		    ELEMENT(NUMBER,10)=AREA
	    END DO
4	    WRITE (1,104) "PRESS ANY KEY TO PROCEED "
	    VERIFY= GETCHARQQ ( ) 
	    call clearscreen($gclearscreen)							!CLEARS THE SCREEN

!********************************************************************************************************************************************
!ELEMENTS MOMENT OF INERTIA------------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************

	    WRITE (1,102) "SECTION PROPERTIES", "------------------", "MOMENT OF INERTIA"	
	    NUM=NUM_ELEM											!MEMBERS WITH A DIFFERENT MOMENT OF INERTIA
	    DO I=1,NUM
		    WRITE (1,103) "ELEMENT NUMBER", I,":"
		    NUMBER=I
		    WRITE (1,100) "MOMENT OF INERTIA? (mm^4) "
		    READ (1,*) MI
		    ELEMENT(NUMBER,9)=MI
	    END DO
	    WRITE (1,104) "PRESS ANY KEY TO PROCEED "
	    VERIFY= GETCHARQQ ( ) 
	    call clearscreen($gclearscreen)							!CLEARS THE SCREEN

!********************************************************************************************************************************************
!ELEMENTS MODULUS OF ELASTICITY--------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************

	    WRITE (1,102) "MEMBER PROPERTIES", "------------------", "MODULUS OF ELASTICITY"
	    NUM=NUM_ELEM
	    DO I=1,NUM
		    WRITE (1,103) "ELEMENT NUMBER", I,":"
		    NUMBER=I
		    WRITE (1,100) "MODULUS OF ELASTICITY? (MPa) "
		    READ (1,*) E
		    ELEMENT(NUMBER,8)=E
	    END DO
	    WRITE (1,104) "PRESS RETURN TO PROCEED "
	    VERIFY= GETCHARQQ ( ) 
	    call clearscreen($gclearscreen)							!CLEARS THE SCREEN

!********************************************************************************************************************************************
!DATA VERIFICATION---------------------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************

	    WRITE (1,102) "DATA VERIFICATION", "-----------------"
	    WRITE (1,103) "THE MEMBERS SECTION PROPERTIES ARE AS FOLLOWS:"								!SECTION PROPERTIES VERIFICATION
	    WRITE (1,106) "NO.","E (MPa)","I (mm^4)","   A (mm^2)"
	    WRITE (1,101) "---------------------------------------------------------------------------"
	    DO I=1,NUM_ELEM
		    WRITE (1,107) ELEMENT(I,1), (ELEMENT(I,J),J=8,10)
	    END DO
3		    WRITE (1,104) "DO YOU VERIFY ALL THE DISPLAYED SECTION PROPERTIES? <Y/N> : "
	    VERIFY= GETCHARQQ ( ) 
	    IF (VERIFY=="Y".OR.VERIFY=="y") THEN
		    GOTO 1
	    ELSE IF (VERIFY=="N".OR.VERIFY=="n") THEN
		    call clearscreen($gclearscreen)
		    GOTO 65
	    ELSE
		    GOTO 3
	    END IF
1	    call clearscreen($gclearscreen)
	    DOFBACKUP=DOF
!********************************************************************************************************************************************
!SUPPORTS------------------------------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************
	
17	    WRITE (1,102) "SUPPORTS", "--------"
	    WRITE (1,103) "SUPPORT TYPES:"
	    WRITE (1,101) "2: PIN SUPPORT"
	    WRITE (1,101) "3: ROLLER SUPPORT"
	    WRITE (1,101) "4: FIXED"
	    WRITE (1,101) "5: FREELY SLIDING GUIDE"
	    WRITE (1,101) "----------------------------------------"
	    WRITE (1,104) "ENTER THE NUMBER OF SUPPORTED NODES: "
	    READ (1,*) NUM_SUP
	    DO I=1,NUM_SUP
		    WRITE (1,104) "THE NUMBER OF NODE: "
		    READ (1,*) N
88		    WRITE (1,100) "TYPE OF SUPPORT:"
		    READ (1,*) SUPPORT
		    SELECT CASE (SUPPORT)
			    CASE (2)
				    DOF(3*N-2)=0								!X DOF
				    DOF(3*N-1)=0								!Y DOF
				    DOF(3*N)=1									!ROTATION DOF
				    J=FOCUSQQ(2)								!SELECTS WINDOW NUMBER 2 (GRAPHICS)
				    L=LENGHT
				    H=HEIGHT
				    SUPPORT=PIN(NODE(N,1),NODE(N,2),L,H)		!DRAWS THE SUPPORT
				    J=FOCUSQQ(1)								!BACK TO SCREEN NUMBER 1 (DATA)	
			    CASE (3)
31				    WRITE (1,100) "VERTICAL OR HORIZONTAL OR TILTED? (V/H/T)"
				    VERIFY= GETCHARQQ ( ) 
				    IF (VERIFY=="V".OR.VERIFY=="v") THEN
					    DOF(3*N-2)=0								!X DOF
					    DOF(3*N-1)=1								!Y DOF
					    DOF(3*N)=1									!ROTATION DOF
					    J=FOCUSQQ(2)								!SELECTS WINDOW NUMBER 2 (GRAPHICS)
					    L=LENGHT
					    H=HEIGHT
					    SUPPORT=ROLLERV(NODE(N,1),NODE(N,2),L,H)	!DRAWS THE SUPPORT
					    J=FOCUSQQ(1)								!BACK TO SCREEN NUMBER 1 (DATA)	
				    ELSE IF (VERIFY=="H".OR.VERIFY=="h") THEN
					    DOF(3*N-2)=1								!X DOF
					    DOF(3*N-1)=0								!Y DOF
					    DOF(3*N)=1									!ROTATION DOF
					    J=FOCUSQQ(2)								!SELECTS WINDOW NUMBER 2 (GRAPHICS)
					    L=LENGHT
					    H=HEIGHT
					    SUPPORT=ROLLERH(NODE(N,1),NODE(N,2),L,H)	!DRAWS THE SUPPORT
					    J=FOCUSQQ(1)								!BACK TO SCREEN NUMBER 1 (DATA)	
!				ELSE IF (VERIFY=="T".OR.VERIFY=="t") THEN
!					DOF(3*N-2)=1								!X DOF
!					DOF(3*N-1)=1								!Y DOF
!					DOF(3*N)=1									!ROTATION DOF
!					J=FOCUSQQ(2)								!SELECTS WINDOW NUMBER 2 (GRAPHICS)
!					L=LENGHT
!					H=HEIGHT
!					SUPPORT=ROLLERV(NODE(N,1),NODE(N,2),L,H)
!					SUPPORT=ROLLERH(NODE(N,1),NODE(N,2),L,H)
!					J=FOCUSQQ(1)
				    ELSE
					    WRITE(1,101), "TYPING MISTAKE MATE! TRY AGAIN."
					    GOTO 31		
				    END IF
			    CASE (4)
				    DOF(3*N-2)=0								!X DOF
				    DOF(3*N-1)=0								!Y DOF
				    DOF(3*N)=0									!ROTATION DOF
				    J=FOCUSQQ(2)								!SELECTS WINDOW NUMBER 2 (GRAPHICS)
				    L=LENGHT
				    H=HEIGHT
				    SUPPORT=FIXED(NODE(N,1),NODE(N,2),L,H)		!DRAWS THE SUPPORT
				    J=FOCUSQQ(1)
			    CASE (5)
32				    WRITE (1,100) "VERTICAL OR HORIZONTAL OR TILTED? (V/H/T)"
				    VERIFY= GETCHARQQ ( ) 
				    IF (VERIFY=="V".OR.VERIFY=="v") THEN
					    DOF(3*N-2)=0								!X DOF
					    DOF(3*N-1)=1								!Y DOF
					    DOF(3*N)=0									!ROTATION DOF
					    J=FOCUSQQ(2)								!SELECTS WINDOW NUMBER 2 (GRAPHICS)
					    L=LENGHT
					    H=HEIGHT
					    SUPPORT=GUIDEV(NODE(N,1),NODE(N,2),L,H)		!DRAWS THE SUPPORT
					    J=FOCUSQQ(1)								!BACK TO SCREEN NUMBER 1 (DATA)	
				    ELSE IF (VERIFY=="H".OR.VERIFY=="h") THEN
					    DOF(3*N-2)=1								!X DOF
					    DOF(3*N-1)=0								!Y DOF
					    DOF(3*N)=0									!ROTATION DOF
					    J=FOCUSQQ(2)								!SELECTS WINDOW NUMBER 2 (GRAPHICS)
					    L=LENGHT
					    H=HEIGHT
					    SUPPORT=GUIDEH(NODE(N,1),NODE(N,2),L,H)		!DRAWS THE SUPPORT
					    J=FOCUSQQ(1)								!BACK TO SCREEN NUMBER 1 (DATA)	
!				ELSE IF (VERIFY=="T".OR.VERIFY=="t") THEN
!					DOF(3*N-2)=1								!X DOF
!					DOF(3*N-1)=1								!Y DOF
!					DOF(3*N)=0									!ROTATION DOF
!					J=FOCUSQQ(2)								!SELECTS WINDOW NUMBER 2 (GRAPHICS)
!					L=LENGHT
!					H=HEIGHT
!					SUPPORT=GUIDEV(NODE(N,1),NODE(N,2),L,H)		!DRAWS THE SUPPORT
!					SUPPORT=GUIDEH(NODE(N,1),NODE(N,2),L,H)
!					J=FOCUSQQ(1)
				    ELSE
					    WRITE(1,101) "TYPING MISTAKE! TRY AGAIN."
					    GOTO 32
				    END IF				
		    CASE DEFAULT
			    WRITE (1,*) "ARE YOU KIDDING ME? PAY MORE ATTENTION TO WHAT YOU TYPE!"
			    GOTO 88
		    END SELECT
	    END DO
18	    WRITE (1,104) "DO YOU VERIFY ALL THE DISPLAYED SUPPORTS <Y/N> : "
	    VERIFY= GETCHARQQ ( ) 
	    IF (VERIFY=="Y".OR.VERIFY=="y") THEN
		    GOTO 16
	    ELSE IF (VERIFY=="N".OR.VERIFY=="n") THEN
		    J=FOCUSQQ(2)
		    call clearscreen($gclearscreen)
		    L=LENGHT
		    H=HEIGHT
		    CALL SETLINESTYLE(#F0F0)						!SET LINE STYLE TO DOTTED LINE
		    I=SETCOLOR (8)									!SETS GRIDLINE COLOR TO "GREY"
		    G=GRID(2*(L+1),2*(H+1))							!DRAWS GRIDLINES
		    CALL SETLINESTYLE(#FFFF)						!SET LINE STYLE TO SIMPLE LINE
		    I=SETCOLOR (4)									!SETS GRAPH COLOR TO "RED"
		    DO I=1,NUM_ELEM
			    L=LINE(ELEMENT(I,2),ELEMENT(I,3),ELEMENT(I,4),ELEMENT(I,5))
	    	END DO
		    J=FOCUSQQ(1)
		    call clearscreen($gclearscreen)
		    DOF=DOFBACKUP
		    GOTO 17
    	ELSE
	    	GOTO 18
	    END IF
16	    call clearscreen($gclearscreen)
	    DOFBACKUP=DOF
	    FEM=0	
	    FEMTEMP=0

!********************************************************************************************************************************************
!CALCULATIONS1--------------------------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************
        ELEMENT_BACKUP=ELEMENT	
	    K=0
	    DO I=1,NUM_ELEM
		    KG=0
        	CALL KGELOBAL(ELEMENT(I,8),ELEMENT(I,9),ELEMENT(I,10),ELEMENT(I,6)*1000,COS(ELEMENT(I,7)),SIN(ELEMENT(I,7)),KG)

		    !ASSEMBLAGE OF GLOBAL STIFFNESS MATRIX
		    !FIRST COLUMN
		    K(3*ELEMENT(I,11)-2,3*ELEMENT(I,11)-2)=K(3*ELEMENT(I,11)-2,3*ELEMENT(I,11)-2)+KG(1,1)
		    K(3*ELEMENT(I,11)-1,3*ELEMENT(I,11)-2)=K(3*ELEMENT(I,11)-1,3*ELEMENT(I,11)-2)+KG(2,1)
		    K(3*ELEMENT(I,11)-0,3*ELEMENT(I,11)-2)=K(3*ELEMENT(I,11)-0,3*ELEMENT(I,11)-2)+KG(3,1)
		    K(3*ELEMENT(I,12)-2,3*ELEMENT(I,11)-2)=K(3*ELEMENT(I,12)-2,3*ELEMENT(I,11)-2)+KG(4,1)
		    K(3*ELEMENT(I,12)-1,3*ELEMENT(I,11)-2)=K(3*ELEMENT(I,12)-1,3*ELEMENT(I,11)-2)+KG(5,1)
		    K(3*ELEMENT(I,12)-0,3*ELEMENT(I,11)-2)=K(3*ELEMENT(I,12)-0,3*ELEMENT(I,11)-2)+KG(6,1)
		    !SECOND COLUMN
		    K(3*ELEMENT(I,11)-2,3*ELEMENT(I,11)-1)=K(3*ELEMENT(I,11)-2,3*ELEMENT(I,11)-1)+KG(1,2)
		    K(3*ELEMENT(I,11)-1,3*ELEMENT(I,11)-1)=K(3*ELEMENT(I,11)-1,3*ELEMENT(I,11)-1)+KG(2,2)
		    K(3*ELEMENT(I,11)-0,3*ELEMENT(I,11)-1)=K(3*ELEMENT(I,11)-0,3*ELEMENT(I,11)-1)+KG(3,2)
		    K(3*ELEMENT(I,12)-2,3*ELEMENT(I,11)-1)=K(3*ELEMENT(I,12)-2,3*ELEMENT(I,11)-1)+KG(4,2)
		    K(3*ELEMENT(I,12)-1,3*ELEMENT(I,11)-1)=K(3*ELEMENT(I,12)-1,3*ELEMENT(I,11)-1)+KG(5,2)
		    K(3*ELEMENT(I,12)-0,3*ELEMENT(I,11)-1)=K(3*ELEMENT(I,12)-0,3*ELEMENT(I,11)-1)+KG(6,2)
		    !THIRD COLUMN
		    K(3*ELEMENT(I,11)-2,3*ELEMENT(I,11)-0)=K(3*ELEMENT(I,11)-2,3*ELEMENT(I,11)-0)+KG(1,3)
		    K(3*ELEMENT(I,11)-1,3*ELEMENT(I,11)-0)=K(3*ELEMENT(I,11)-1,3*ELEMENT(I,11)-0)+KG(2,3)
	    	K(3*ELEMENT(I,11)-0,3*ELEMENT(I,11)-0)=K(3*ELEMENT(I,11)-0,3*ELEMENT(I,11)-0)+KG(3,3)
    		K(3*ELEMENT(I,12)-2,3*ELEMENT(I,11)-0)=K(3*ELEMENT(I,12)-2,3*ELEMENT(I,11)-0)+KG(4,3)
    		K(3*ELEMENT(I,12)-1,3*ELEMENT(I,11)-0)=K(3*ELEMENT(I,12)-1,3*ELEMENT(I,11)-0)+KG(5,3)
    		K(3*ELEMENT(I,12)-0,3*ELEMENT(I,11)-0)=K(3*ELEMENT(I,12)-0,3*ELEMENT(I,11)-0)+KG(6,3)
    		!FOURTH COLUMN
    		K(3*ELEMENT(I,11)-2,3*ELEMENT(I,12)-2)=K(3*ELEMENT(I,11)-2,3*ELEMENT(I,12)-2)+KG(1,4)
    		K(3*ELEMENT(I,11)-1,3*ELEMENT(I,12)-2)=K(3*ELEMENT(I,11)-1,3*ELEMENT(I,12)-2)+KG(2,4)
    		K(3*ELEMENT(I,11)-0,3*ELEMENT(I,12)-2)=K(3*ELEMENT(I,11)-0,3*ELEMENT(I,12)-2)+KG(3,4)
    		K(3*ELEMENT(I,12)-2,3*ELEMENT(I,12)-2)=K(3*ELEMENT(I,12)-2,3*ELEMENT(I,12)-2)+KG(4,4)
    		K(3*ELEMENT(I,12)-1,3*ELEMENT(I,12)-2)=K(3*ELEMENT(I,12)-1,3*ELEMENT(I,12)-2)+KG(5,4)
    		K(3*ELEMENT(I,12)-0,3*ELEMENT(I,12)-2)=K(3*ELEMENT(I,12)-0,3*ELEMENT(I,12)-2)+KG(6,4)
    		!FIFTH COLUMN
    		K(3*ELEMENT(I,11)-2,3*ELEMENT(I,12)-1)=K(3*ELEMENT(I,11)-2,3*ELEMENT(I,12)-1)+KG(1,5)
    		K(3*ELEMENT(I,11)-1,3*ELEMENT(I,12)-1)=K(3*ELEMENT(I,11)-1,3*ELEMENT(I,12)-1)+KG(2,5)
    		K(3*ELEMENT(I,11)-0,3*ELEMENT(I,12)-1)=K(3*ELEMENT(I,11)-0,3*ELEMENT(I,12)-1)+KG(3,5)
    		K(3*ELEMENT(I,12)-2,3*ELEMENT(I,12)-1)=K(3*ELEMENT(I,12)-2,3*ELEMENT(I,12)-1)+KG(4,5)
    		K(3*ELEMENT(I,12)-1,3*ELEMENT(I,12)-1)=K(3*ELEMENT(I,12)-1,3*ELEMENT(I,12)-1)+KG(5,5)
    		K(3*ELEMENT(I,12)-0,3*ELEMENT(I,12)-1)=K(3*ELEMENT(I,12)-0,3*ELEMENT(I,12)-1)+KG(6,5)
    		!SIXTH COLUMN
    		K(3*ELEMENT(I,11)-2,3*ELEMENT(I,12)-0)=K(3*ELEMENT(I,11)-2,3*ELEMENT(I,12)-0)+KG(1,6)
    		K(3*ELEMENT(I,11)-1,3*ELEMENT(I,12)-0)=K(3*ELEMENT(I,11)-1,3*ELEMENT(I,12)-0)+KG(2,6)
    		K(3*ELEMENT(I,11)-0,3*ELEMENT(I,12)-0)=K(3*ELEMENT(I,11)-0,3*ELEMENT(I,12)-0)+KG(3,6)
    		K(3*ELEMENT(I,12)-2,3*ELEMENT(I,12)-0)=K(3*ELEMENT(I,12)-2,3*ELEMENT(I,12)-0)+KG(4,6)
    		K(3*ELEMENT(I,12)-1,3*ELEMENT(I,12)-0)=K(3*ELEMENT(I,12)-1,3*ELEMENT(I,12)-0)+KG(5,6)
    		K(3*ELEMENT(I,12)-0,3*ELEMENT(I,12)-0)=K(3*ELEMENT(I,12)-0,3*ELEMENT(I,12)-0)+KG(6,6)
    	END DO

    	COUNT=0
    	DO I=1, 3*NUM_NODE
    		IF(DOF(I)==1) COUNT=COUNT+1
    	END DO

    	ALLOCATE(KF(COUNT,COUNT), FEMF(COUNT),KFS(COUNT,3*NUM_NODE-COUNT),DELTAF(COUNT),DELTAS(3*NUM_NODE-COUNT),KFINV(COUNT,COUNT))
    	FEM_EL=0 
!********************************************************************************************************************************************
!LOADING-------------------------------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************
22	    WRITE (1,102) "LOADING", "-------"	
	    WRITE (1,103) "PLEASE NOTE THAT YOU MAY NEED TO USE THE PRINCIPLE OF SUPERPOSITION."
	    WRITE (1,103) "ALSO NOTE THAT ALL LOADS AND LENGHTS ARE IN ELEMENT LOCAL CO-ORDINATES."
	    WRITE (1,103) "LOAD TYPES:"
	    WRITE (1,103) "1: CONCENTRATED PERPENDICULAR LOAD"
	    WRITE (1,101) "2: DISTRIBUTED UNIFORM LOAD"
	    WRITE (1,101) "3: WHOLE-SPAN DISTRIBUTED TRIANGULAR LOAD"
	    WRITE (1,101) "4: CONCENTRATED MOMENT"
	    WRITE (1,101) "5: CONCENTRATED TANGENTIAL LOAD"
	    WRITE (1,101) "6: DISTRIBUTED TANGENTIAL LOAD"
	    WRITE (1,101) "----------------------------------------"
	    WRITE (1,104) "ENTER THE NUMBER OF LOADS:"
	    READ (1,*) NUM_LOAD
	    DO I=1,NUM_LOAD
		    WRITE (1,104) "THE NUMBER OF ELEMENT: "
		    READ (1,*) N
19		    WRITE (1,100) "LOAD TYPE: "
		    READ (1,*) LD
		    SELECT CASE (LD)
			    CASE (1)
				    WRITE(1,100) "LOAD MAGNITUDE? (N) "
				    READ(1,*) P
				    WRITE(1,100) "DISTANCE FROM THE FIRST NODE OF ELEMENT? (mm) "
				    READ(1,*) AA
				    L=ELEMENT(N,6)*1000
				    BB=L-AA							
				    FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11)-2)+0								!FIRST NODE FX
				    FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)+(P*BB*BB*(3*AA+BB))/(L**3)	!FIRST NODE FY
				    FEMTEMP(3*ELEMENT(N,11))=FEMTEMP(3*ELEMENT(N,11))+1*P*BB*BB*AA/(L*L)				!FIRST NODE MOMENT	
				    FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-2)+0								!SECOND NODE FX
				    FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)+(P*AA*AA*(3*BB+AA))/(L**3)	!SECOND NODE FY
				    FEMTEMP(3*ELEMENT(N,12))=FEMTEMP(3*ELEMENT(N,12))+(-1)*P*AA*AA*BB/(L*L)					!SECOND NODE MOMENT
				    !CONVERSION TO THE GLOBAL SYSTEM
				    FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11)-1)*ELEMENT(N,18)+FEMTEMP(3*ELEMENT(N,11)-2)*ELEMENT(N,17)
				    FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)*ELEMENT(N,17)-FEMTEMP(3*ELEMENT(N,11)-2)*ELEMENT(N,18)
				    FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-1)*ELEMENT(N,18)+FEMTEMP(3*ELEMENT(N,12)-2)*ELEMENT(N,17)
				    FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)*ELEMENT(N,17)-FEMTEMP(3*ELEMENT(N,12)-2)*ELEMENT(N,18)
				    !ADDITION TO OTHER FEMs
				    FEM(3*ELEMENT(N,11)-2)=FEM(3*ELEMENT(N,11)-2)+FEMTEMP(3*ELEMENT(N,11)-2)			!FIRST NODE FX
				    FEM(3*ELEMENT(N,11)-1)=FEM(3*ELEMENT(N,11)-1)+FEMTEMP(3*ELEMENT(N,11)-1)			!FIRST NODE FY
				    FEM(3*ELEMENT(N,11))=FEM(3*ELEMENT(N,11))+FEMTEMP(3*ELEMENT(N,11))					!FIRST NODE MOMENT	
				    FEM(3*ELEMENT(N,12)-2)=FEM(3*ELEMENT(N,12)-2)+FEMTEMP(3*ELEMENT(N,12)-2)			!SECOND NODE FX
				    FEM(3*ELEMENT(N,12)-1)=FEM(3*ELEMENT(N,12)-1)+FEMTEMP(3*ELEMENT(N,12)-1)			!SECOND NODE FY
				    FEM(3*ELEMENT(N,12))=FEM(3*ELEMENT(N,12))+FEMTEMP(3*ELEMENT(N,12))					!SECOND NODE MOMENT
				    FEM_EL(1:6,N)=FEM_EL(1:6,N)+FEMTEMP(3*ELEMENT(N,11)-2:3*ELEMENT(N,12))              !NEWWWWWWWW
				    FEMTEMP=0					
			    CASE (2)
				    WRITE(1,100) "LOAD MAGNITUDE? (N/mm) "
				    READ(1,*) W
				    WRITE(1,100) "LOAD LENGHT? (mm) "
				    READ(1,*) D
				    WRITE(1,100) "DISTANCE OF THE BEGINING OF THE LOAD FROM THE FIRST NODE OF THE ELEMENT? (mm) "
				    READ(1,*) AA
				    L=ELEMENT(N,6)*1000
				    AA=AA+(D/2)
				    BB=L-AA								
				    FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11)-2)+0								!FIRST NODE FX
				    FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)+(W*D)/(L**3)*((2*AA+L)*BB*BB+((AA-BB)*D*D)/4)		!FIRST NODE FY
				    FEMTEMP(3*ELEMENT(N,11))=FEMTEMP(3*ELEMENT(N,11))+1*(W*D/(L*L))*(AA*BB*BB+(AA-2*BB)*D*D/12)			!FIRST NODE MOMENT	
				    FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-2)+0								!SECOND NODE FX
				    FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)+(W*D)/(L**3)*((2*BB+L)*AA*AA-((AA-BB)*D*D)/4)		!SECOND NODE FY
				    FEMTEMP(3*ELEMENT(N,12))=FEMTEMP(3*ELEMENT(N,12))+(-1)*(W*D/(L*L))*(AA*AA*BB+(BB-2*AA)*D*D/12)			!SECOND NODE MOMENT	
				!CONVERSION TO THE GLOBAL SYSTEM
				FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11)-1)*ELEMENT(N,18)+FEMTEMP(3*ELEMENT(N,11)-2)*ELEMENT(N,17)
				FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)*ELEMENT(N,17)-FEMTEMP(3*ELEMENT(N,11)-2)*ELEMENT(N,18)
				FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-1)*ELEMENT(N,18)+FEMTEMP(3*ELEMENT(N,12)-2)*ELEMENT(N,17)
				FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)*ELEMENT(N,17)-FEMTEMP(3*ELEMENT(N,12)-2)*ELEMENT(N,18)
				!ADDITION TO OTHER FEMs
				FEM(3*ELEMENT(N,11)-2)=FEM(3*ELEMENT(N,11)-2)+FEMTEMP(3*ELEMENT(N,11)-2)			!FIRST NODE FX
				FEM(3*ELEMENT(N,11)-1)=FEM(3*ELEMENT(N,11)-1)+FEMTEMP(3*ELEMENT(N,11)-1)			!FIRST NODE FY
				FEM(3*ELEMENT(N,11))=FEM(3*ELEMENT(N,11))+FEMTEMP(3*ELEMENT(N,11))				!FIRST NODE MOMENT	
				FEM(3*ELEMENT(N,12)-2)=FEM(3*ELEMENT(N,12)-2)+FEMTEMP(3*ELEMENT(N,12)-2)			!SECOND NODE FX
				FEM(3*ELEMENT(N,12)-1)=FEM(3*ELEMENT(N,12)-1)+FEMTEMP(3*ELEMENT(N,12)-1)			!SECOND NODE FY
				FEM(3*ELEMENT(N,12))=FEM(3*ELEMENT(N,12))+FEMTEMP(3*ELEMENT(N,12))				!SECOND NODE MOMENT
				FEM_EL(1:6,N)=FEM_EL(1:6,N)+FEMTEMP(3*ELEMENT(N,11)-2:3*ELEMENT(N,12))          !NEWWWWWWWWWW
				FEMTEMP=0					
			CASE (3)
				WRITE(1,100) "MAXIMUM MAGNITUDE OF LOAD? (N) "
				READ(1,*) W
				L=ELEMENT(N,6)*1000
20				WRITE(1,100) "IS THE ZERO PART TO THE RIGHT? <Y/N> "
				VERIFY= GETCHARQQ ( ) 
				IF (VERIFY=="Y".OR.VERIFY=="y") THEN
					FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11)-2)+0							!FIRST NODE FX
					FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)+(7*W*L)/20					!FIRST NODE FY
					FEMTEMP(3*ELEMENT(N,11))=FEMTEMP(3*ELEMENT(N,11))+1*W*L*L/20						!FIRST NODE MOMENT	
					FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-2)+0							!SECOND NODE FX
					FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)+(3*W*L)/20					!SECOND NODE FY
					FEMTEMP(3*ELEMENT(N,12))=FEMTEMP(3*ELEMENT(N,12))+(-1)*W*L*L/30						!SECOND NODE MOMENT			
				ELSE IF (VERIFY=="N".OR.VERIFY=="n") THEN
					FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11)-2)+0							!FIRST NODE FX
					FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)+(3*W*L)/20					!FIRST NODE FY
					FEMTEMP(3*ELEMENT(N,11))=FEMTEMP(3*ELEMENT(N,11))+1*W*L*L/30						!FIRST NODE MOMENT	
					FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-2)+0							!SECOND NODE FX
					FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)+(7*W*L)/20					!SECOND NODE FY
					FEMTEMP(3*ELEMENT(N,12))=FEMTEMP(3*ELEMENT(N,12))+(-1)*W*L*L/20						!SECOND NODE MOMENT		
				ELSE
					GOTO 20
				END IF
				!CONVERSION TO THE GLOBAL SYSTEM
				FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11)-1)*ELEMENT(N,18)+FEMTEMP(3*ELEMENT(N,11)-2)*ELEMENT(N,17)
				FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)*ELEMENT(N,17)-FEMTEMP(3*ELEMENT(N,11)-2)*ELEMENT(N,18)
				FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-1)*ELEMENT(N,18)+FEMTEMP(3*ELEMENT(N,12)-2)*ELEMENT(N,17)
				FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)*ELEMENT(N,17)-FEMTEMP(3*ELEMENT(N,12)-2)*ELEMENT(N,18)
				!ADDITION TO OTHER FEMs
				FEM(3*ELEMENT(N,11)-2)=FEM(3*ELEMENT(N,11)-2)+FEMTEMP(3*ELEMENT(N,11)-2)			!FIRST NODE FX
				FEM(3*ELEMENT(N,11)-1)=FEM(3*ELEMENT(N,11)-1)+FEMTEMP(3*ELEMENT(N,11)-1)			!FIRST NODE FY
				FEM(3*ELEMENT(N,11))=FEM(3*ELEMENT(N,11))+FEMTEMP(3*ELEMENT(N,11))					!FIRST NODE MOMENT	
				FEM(3*ELEMENT(N,12)-2)=FEM(3*ELEMENT(N,12)-2)+FEMTEMP(3*ELEMENT(N,12)-2)			!SECOND NODE FX
				FEM(3*ELEMENT(N,12)-1)=FEM(3*ELEMENT(N,12)-1)+FEMTEMP(3*ELEMENT(N,12)-1)			!SECOND NODE FY
				FEM(3*ELEMENT(N,12))=FEM(3*ELEMENT(N,12))+FEMTEMP(3*ELEMENT(N,12))					!SECOND NODE MOMENT
				FEM_EL(1:6,N)=FEM_EL(1:6,N)+FEMTEMP(3*ELEMENT(N,11)-2:3*ELEMENT(N,12))              !NEWWWWWW
				FEMTEMP=0							
			CASE (4)
				WRITE(1,100) "MOMENT MAGNITUDE? (N.mm) (CLOCKWISE MOMENT IS CONSIDERED AS POSITIVE) "
				READ(1,*) M
				WRITE(1,100) "DISTANCE FROM THE FIRST NODE OF ELEMENT? (mm) "
				READ(1,*) AA
				L=ELEMENT(N,6)*1000
				BB=L-AA
				FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11)-2)+0								!FIRST NODE FX
				FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)-(2*M*AA*BB)/(L**3)			!FIRST NODE FY
				FEMTEMP(3*ELEMENT(N,11))=FEMTEMP(3*ELEMENT(N,11))+(-1)*(M*BB*(2*AA-BB))/(L**2)		!FIRST NODE MOMENT	
				FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-2)+0								!SECOND NODE FX
				FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)+(2*M*AA*BB)/(L**3)			!SECOND NODE FY
				FEMTEMP(3*ELEMENT(N,12))=FEMTEMP(3*ELEMENT(N,12))+(-1)*(M*AA*(2*BB-AA))/(L**2)		!SECOND NODE MOMENT	
				!CONVERSION TO THE GLOBAL SYSTEM
				FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11)-1)*ELEMENT(N,18)+FEMTEMP(3*ELEMENT(N,11)-2)*ELEMENT(N,17)
				FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)*ELEMENT(N,17)-FEMTEMP(3*ELEMENT(N,11)-2)*ELEMENT(N,18)
				FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-1)*ELEMENT(N,18)+FEMTEMP(3*ELEMENT(N,12)-2)*ELEMENT(N,17)
				FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)*ELEMENT(N,17)-FEMTEMP(3*ELEMENT(N,12)-2)*ELEMENT(N,18)
				!ADDITION TO OTHER FEMs
				FEM(3*ELEMENT(N,11)-2)=FEM(3*ELEMENT(N,11)-2)+FEMTEMP(3*ELEMENT(N,11)-2)			!FIRST NODE FX
				FEM(3*ELEMENT(N,11)-1)=FEM(3*ELEMENT(N,11)-1)+FEMTEMP(3*ELEMENT(N,11)-1)			!FIRST NODE FY
				FEM(3*ELEMENT(N,11))=FEM(3*ELEMENT(N,11))+FEMTEMP(3*ELEMENT(N,11))					!FIRST NODE MOMENT	
				FEM(3*ELEMENT(N,12)-2)=FEM(3*ELEMENT(N,12)-2)+FEMTEMP(3*ELEMENT(N,12)-2)			!SECOND NODE FX
				FEM(3*ELEMENT(N,12)-1)=FEM(3*ELEMENT(N,12)-1)+FEMTEMP(3*ELEMENT(N,12)-1)			!SECOND NODE FY
				FEM(3*ELEMENT(N,12))=FEM(3*ELEMENT(N,12))+FEMTEMP(3*ELEMENT(N,12))				!SECOND NODE MOMENT
				FEM_EL(1:6,N)=FEM_EL(1:6,N)+FEMTEMP(3*ELEMENT(N,11)-2:3*ELEMENT(N,12))          !NEWWWWWWWW
				FEMTEMP=0					
			CASE (5)
				WRITE(1,100) "LOAD MAGNITUDE? (N) "
				READ(1,*) P
				WRITE(1,100) "LOAD DISTANCE FROM THE FIRST NODE OF THE ELEMENT? (mm) "
				READ(1,*) AA
				L=ELEMENT(N,6)*1000
				BB=L-AA	
				FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11)-2)+BB*P/L					!FIRST NODE FX
				FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)+0						!FIRST NODE FY
				FEMTEMP(3*ELEMENT(N,11))=FEMTEMP(3*ELEMENT(N,11))+0						!FIRST NODE MOMENT	
				FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-2)+AA*P/L					!SECOND NODE FX
				FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)+0						!SECOND NODE FY
				FEMTEMP(3*ELEMENT(N,12))=FEMTEMP(3*ELEMENT(N,12))+0						!SECOND NODE MOMENT
				!CONVERSION TO THE GLOBAL SYSTEM
				FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11)-1)*ELEMENT(N,18)+FEMTEMP(3*ELEMENT(N,11)-2)*ELEMENT(N,17)
				FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)*ELEMENT(N,17)-FEMTEMP(3*ELEMENT(N,11)-2)*ELEMENT(N,18)
				FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-1)*ELEMENT(N,18)+FEMTEMP(3*ELEMENT(N,12)-2)*ELEMENT(N,17)
				FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)*ELEMENT(N,17)-FEMTEMP(3*ELEMENT(N,12)-2)*ELEMENT(N,18)
				!ADDITION TO OTHER FEMs
				FEM(3*ELEMENT(N,11)-2)=FEM(3*ELEMENT(N,11)-2)+FEMTEMP(3*ELEMENT(N,11)-2)			!FIRST NODE FX
				FEM(3*ELEMENT(N,11)-1)=FEM(3*ELEMENT(N,11)-1)+FEMTEMP(3*ELEMENT(N,11)-1)			!FIRST NODE FY
				FEM(3*ELEMENT(N,11))=FEM(3*ELEMENT(N,11))+FEMTEMP(3*ELEMENT(N,11))				!FIRST NODE MOMENT	
				FEM(3*ELEMENT(N,12)-2)=FEM(3*ELEMENT(N,12)-2)+FEMTEMP(3*ELEMENT(N,12)-2)			!SECOND NODE FX
				FEM(3*ELEMENT(N,12)-1)=FEM(3*ELEMENT(N,12)-1)+FEMTEMP(3*ELEMENT(N,12)-1)			!SECOND NODE FY
				FEM(3*ELEMENT(N,12))=FEM(3*ELEMENT(N,12))+FEMTEMP(3*ELEMENT(N,12))				!SECOND NODE MOMENT
				FEM_EL(1:6,N)=FEM_EL(1:6,N)+FEMTEMP(3*ELEMENT(N,11)-2:3*ELEMENT(N,12))          !NEWWWWWWW
				FEMTEMP=0					
			CASE (6)
				WRITE(1,100) "LOAD MAGNITUDE? (N/mm) "
				READ(1,*) W
				WRITE(1,100) "LOAD LENGHT? (mm) "
				READ(1,*) D
				WRITE(1,100) "DISTANCE OF THE BEGINING OF THE LOAD FROM THE FIRST NODE OF THE ELEMENT? (mm) "
				READ(1,*) AA
				L=ELEMENT(N,6)*1000
				AA=AA+(D/2)
				BB=L-AA-(D/2)							
				FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11)-2)+W*D*BB/L					!FIRST NODE FX
				FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)+0						!FIRST NODE FY
				FEMTEMP(3*ELEMENT(N,11))=FEMTEMP(3*ELEMENT(N,11))+0						!FIRST NODE MOMENT	
				FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-2)+W*D*AA/L					!SECOND NODE FX
				FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)+0						!SECOND NODE FY
				FEMTEMP(3*ELEMENT(N,12))=FEMTEMP(3*ELEMENT(N,12))+0						!SECOND NODE MOMENT	
				!CONVERSION TO THE GLOBAL SYSTEM
				FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11)-1)*ELEMENT(N,18)+FEMTEMP(3*ELEMENT(N,11)-2)*ELEMENT(N,17)
				FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)*ELEMENT(N,17)-FEMTEMP(3*ELEMENT(N,11)-2)*ELEMENT(N,18)
				FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-1)*ELEMENT(N,18)+FEMTEMP(3*ELEMENT(N,12)-2)*ELEMENT(N,17)
				FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)*ELEMENT(N,17)-FEMTEMP(3*ELEMENT(N,12)-2)*ELEMENT(N,18)
				!ADDITION TO OTHER FEMs
				FEM(3*ELEMENT(N,11)-2)=FEM(3*ELEMENT(N,11)-2)+FEMTEMP(3*ELEMENT(N,11)-2)			!FIRST NODE FX
				FEM(3*ELEMENT(N,11)-1)=FEM(3*ELEMENT(N,11)-1)+FEMTEMP(3*ELEMENT(N,11)-1)			!FIRST NODE FY
				FEM(3*ELEMENT(N,11))=FEM(3*ELEMENT(N,11))+FEMTEMP(3*ELEMENT(N,11))				!FIRST NODE MOMENT	
				FEM(3*ELEMENT(N,12)-2)=FEM(3*ELEMENT(N,12)-2)+FEMTEMP(3*ELEMENT(N,12)-2)			!SECOND NODE FX
				FEM(3*ELEMENT(N,12)-1)=FEM(3*ELEMENT(N,12)-1)+FEMTEMP(3*ELEMENT(N,12)-1)			!SECOND NODE FY
				FEM(3*ELEMENT(N,12))=FEM(3*ELEMENT(N,12))+FEMTEMP(3*ELEMENT(N,12))				!SECOND NODE MOMENT
				FEM_EL(1:6,N)=FEM_EL(1:6,N)+FEMTEMP(3*ELEMENT(N,11)-2:3*ELEMENT(N,12))          !NEWWWWWWWWWWW
				FEMTEMP=0									
			CASE DEFAULT
				WRITE (1,*) "ARE YOU KIDDING ME? PAY MORE ATTENTION TO WHAT YOU TYPE!"
				GOTO 19
		    END SELECT
	    END DO

23	    WRITE (1,104) "DO YOU VERIFY ALL THE LOADING DATA YOU JUST ENTERED? <Y/N> : "
	    VERIFY= GETCHARQQ ( ) 
	    IF (VERIFY=="Y".OR.VERIFY=="y") THEN
		    GOTO 21
	    ELSE IF (VERIFY=="N".OR.VERIFY=="n") THEN
		    call clearscreen($gclearscreen)
		    !J=FOCUSQQ(2)
		    !call clearscreen($gclearscreen)
		    !L=LENGHT
		    !H=HEIGHT
		    !CALL SETLINESTYLE(#F0F0)						!SET LINE STYLE TO DOTTED LINE
		    !I=SETCOLOR (8)									!SETS GRIDLINE COLOR TO "GREY"
		    !G=GRID(2*(L+1),2*(H+1))							!DRAWS GRIDLINES
		    !CALL SETLINESTYLE(#FFFF)						!SET LINE STYLE TO SIMPLE LINE
		    !I=SETCOLOR (4)									!SETS GRAPH COLOR TO "RED"
		    !DO I=1,NUM_ELEM
			    !L=LINE(ELEMENT(I,2),ELEMENT(I,3),ELEMENT(I,4),ELEMENT(I,5))
		    !END DO
		    !J=FOCUSQQ(1)
		    FEM=0
		    call clearscreen($gclearscreen)
		    GOTO 22
	    ELSE
		    GOTO 23
	    END IF
21	    call clearscreen($gclearscreen)
	    FEMBACKUP=FEM

!********************************************************************************************************************************************
!TEMPERATURE EFFECT--------------------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************
	    DO I=I,NUM_ELEM
		    ELEMENT(I,6)=ELEMENT(I,6)*1000
	    END DO

24	    WRITE (1,102) "TEMPERATURE EFFECT", "------------------"
	    WRITE (1,104) "ARE THERE ANY TEMPERATURE EFFECTS? <Y/N> "
	    VERIFY= GETCHARQQ ( ) 
	    IF (VERIFY=="Y".OR.VERIFY=="y") THEN
		    WRITE (1,104) "ENTER THE NUMBER OF ELEMENTS WITH A TEMPERATURE EFFECT: "
		    READ (1,*) NUM_TEMP
		    DO I=1,NUM_TEMP
			    WRITE(1,104) "THE NUMBER OF THE ELEMENT? "
			    READ(1,*) N
27			    WRITE(1,100) "IS THE TEMPERATURE EFFECT THE UNIFORM TYPE OR THE GRADIENT TYPE? <U/G> "
			    VERIFY= GETCHARQQ ( ) 
			    IF (VERIFY=="U".OR.VERIFY=="u") THEN
				    WRITE (1,100) "UNIFORM TEMPERATURE CHANGE? (.C) (PAY ATTENTION TO THE POSITIVES AND NEGATIVES) "
				    READ (1,*) UTEMP
				    WRITE (1,100) "THERMAL EXPANSION COEFFICIENT? (1/.C) "
				    READ (1,*) ALPHA
				    L=ELEMENT(N,6)
				    FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11))+ELEMENT(N,8)*ELEMENT(N,10)*ALPHA*UTEMP		!FIRST NODE FX
				    FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)+0							!FIRST NODE FY
				    FEMTEMP(3*ELEMENT(N,11))=FEMTEMP(3*ELEMENT(N,11))+0							!FIRST NODE MOMENT	
				    FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-2)-ELEMENT(N,8)*ELEMENT(N,10)*ALPHA*UTEMP		!SECOND NODE FX
				    FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)+0							!SECOND NODE FY
				    FEMTEMP(3*ELEMENT(N,12))=FEMTEMP(3*ELEMENT(N,12))+0							!SECOND NODE MOMENT	
				    !CONVERSION TO THE GLOBAL SYSTEM
				    FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11)-1)*ELEMENT(N,18)+FEMTEMP(3*ELEMENT(N,11)-2)*ELEMENT(N,17)
				    FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)*ELEMENT(N,17)-FEMTEMP(3*ELEMENT(N,11)-2)*ELEMENT(N,18)
				    FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-1)*ELEMENT(N,18)+FEMTEMP(3*ELEMENT(N,12)-2)*ELEMENT(N,17)
				    FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)*ELEMENT(N,17)-FEMTEMP(3*ELEMENT(N,12)-2)*ELEMENT(N,18)
				    !ADDITION TO OTHER FEMs
				    FEM(3*ELEMENT(N,11)-2)=FEM(3*ELEMENT(N,11)-2)+FEMTEMP(3*ELEMENT(N,11)-2)			!FIRST NODE FX
				    FEM(3*ELEMENT(N,11)-1)=FEM(3*ELEMENT(N,11)-1)+FEMTEMP(3*ELEMENT(N,11)-1)			!FIRST NODE FY
				    FEM(3*ELEMENT(N,11))=FEM(3*ELEMENT(N,11))+FEMTEMP(3*ELEMENT(N,11))				!FIRST NODE MOMENT	
				    FEM(3*ELEMENT(N,12)-2)=FEM(3*ELEMENT(N,12)-2)+FEMTEMP(3*ELEMENT(N,12)-2)			!SECOND NODE FX
				    FEM(3*ELEMENT(N,12)-1)=FEM(3*ELEMENT(N,12)-1)+FEMTEMP(3*ELEMENT(N,12)-1)			!SECOND NODE FY
				    FEM(3*ELEMENT(N,12))=FEM(3*ELEMENT(N,12))+FEMTEMP(3*ELEMENT(N,12))				!SECOND NODE MOMENT
				    FEM_EL(1:6,N)=FEM_EL(1:6,N)+FEMTEMP(3*ELEMENT(N,11)-2:3*ELEMENT(N,12))          !NEWWWWWWWWW
			    	FEMTEMP=0													
			    ELSE IF (VERIFY=="G".OR.VERIFY=="g") THEN
			    	WRITE (1,101) "POSITIVE= HIGHER TEMPERATURE AT THE TOP OF THE ELEMENT"
				    WRITE (1,101) "NEGATIVE= HIGHER TEMPERATURE AT THE BOTTOM OF THE ELEMENT"
			    	WRITE (1,100) "GRADIANT TEMPERATURE CHANGE? (.C) "
			    	READ (1,*) GTEMP
			    	WRITE (1,100) "ELEMENT CROSS-SECTION HEIGHT? (mm)"
			    	READ (1,*) CSH
			    	WRITE (1,100) "THERMAL EXPANSION COEFFICIENT? (1/.C) "
			    	READ (1,*) ALPHA
			    	L=ELEMENT(N,6)
			    	FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11))+0									!FIRST NODE FX
			    	FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)+0									!FIRST NODE FY
			    	FEMTEMP(3*ELEMENT(N,11))=FEMTEMP(3*ELEMENT(N,11))+(ELEMENT(N,8)*ELEMENT(N,9)*ALPHA*GTEMP)/CSH				!FIRST NODE MOMENT	
			    	FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-2)+0									!SECOND NODE FX
		    		FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)+0									!SECOND NODE FY
		    		FEMTEMP(3*ELEMENT(N,12))=FEMTEMP(3*ELEMENT(N,12))+(-1)*(ELEMENT(N,8)*ELEMENT(N,9)*ALPHA*GTEMP)/CSH				!SECOND NODE MOMENT	
		    		!CONVERSION TO THE GLOBAL SYSTEM
		    		FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11)-1)*ELEMENT(N,18)+FEMTEMP(3*ELEMENT(N,11)-2)*ELEMENT(N,17)
		    		FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)*ELEMENT(N,17)-FEMTEMP(3*ELEMENT(N,11)-2)*ELEMENT(N,18)
		       		FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-1)*ELEMENT(N,18)+FEMTEMP(3*ELEMENT(N,12)-2)*ELEMENT(N,17)
		    		FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)*ELEMENT(N,17)-FEMTEMP(3*ELEMENT(N,12)-2)*ELEMENT(N,18)
		    		!ADDITION TO OTHER FEMs
		    		FEM(3*ELEMENT(N,11)-2)=FEM(3*ELEMENT(N,11)-2)+FEMTEMP(3*ELEMENT(N,11)-2)			!FIRST NODE FX
		    		FEM(3*ELEMENT(N,11)-1)=FEM(3*ELEMENT(N,11)-1)+FEMTEMP(3*ELEMENT(N,11)-1)			!FIRST NODE FY
		    		FEM(3*ELEMENT(N,11))=FEM(3*ELEMENT(N,11))+FEMTEMP(3*ELEMENT(N,11))				!FIRST NODE MOMENT	
			    	FEM(3*ELEMENT(N,12)-2)=FEM(3*ELEMENT(N,12)-2)+FEMTEMP(3*ELEMENT(N,12)-2)			!SECOND NODE FX
		    		FEM(3*ELEMENT(N,12)-1)=FEM(3*ELEMENT(N,12)-1)+FEMTEMP(3*ELEMENT(N,12)-1)			!SECOND NODE FY
		    		FEM(3*ELEMENT(N,12))=FEM(3*ELEMENT(N,12))+FEMTEMP(3*ELEMENT(N,12))				!SECOND NODE MOMENT
		    		FEM_EL(1:6,N)=FEM_EL(1:6,N)+FEMTEMP(3*ELEMENT(N,11)-2:3*ELEMENT(N,12))          !NEWWWWWWWW
		    		FEMTEMP=0										
		    	ELSE
			    	WRITE (1,101) "I GUESS YOU MADE A TYPING MISTAKE. TRY AGAIN."
			    	GOTO 27
		    	END IF
	    	END DO
    	ELSE IF (VERIFY=="N".OR.VERIFY=="n") THEN
		    GOTO 25
    	ELSE
	    	WRITE (1,101), "ARE YOU KIDDING ME? PAY MORE ATTENTION TO WHAT YOU TYPE!"
	    	GOTO 24
    	END IF
26      WRITE (1,104) "DO YOU VERIFY ALL THE DATA YOU JUST ENTERED? <Y/N> : "
	    VERIFY= GETCHARQQ ( ) 
	    IF (VERIFY=="Y".OR.VERIFY=="y") THEN
		    GOTO 25
	    ELSE IF (VERIFY=="N".OR.VERIFY=="n") THEN
		    FEM=FEMBACKUP
		    call clearscreen($gclearscreen)
		    GOTO 24
	    ELSE
		    GOTO 26
    	END IF
25	    call clearscreen($gclearscreen)

	    FEMBACKUP=FEM
	    DISPBACKUP=DISP
	    DOFBACKUP=DOF
!********************************************************************************************************************************************
!SUPPORT DEFLECTION--------------------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************
29	    WRITE (1,102) "SUPPORT DEFLECTION", "------------------"
	    WRITE (1,104) "ARE THERE ANY SUPPORT DEFLECTIONS? <Y/N> "
	    VERIFY= GETCHARQQ ( ) 
	    IF (VERIFY=="Y".OR.VERIFY=="y") THEN
		    WRITE (1,104) "ENTER THE NUMBER OF SUPPORTS WITH A DEFLECTION: "
		    READ (1,*) NUM_DEF
		    ALLOCATE(S_DEF(NUM_DEF,3))
		    DO I=1,NUM_DEF
			    WRITE(1,104) "THE NUMBER OF THE NODE WITH THE DEFLECTED SUPPORT: "
			    READ(1,*) N
30				    WRITE (1,100) "IS THE DEFLECTION ROTATIONAL? (NO MEANS PERPENDICULAR DEFLECTION) <Y/N> "
				    VERIFY= GETCHARQQ ( ) 
				    IF (VERIFY=="N".OR.VERIFY=="n") THEN
					    WRITE (1,100) "DEFLECTION (mm)? (PAY ATTENTION TO THE POSITIVES AND NEGATIVES) "
					    READ (1,*) DEF
					    DISP(3*N-1)=DEF
					    DOF(3*N-1)=0
					    S_DEF(I,1)=N
					    S_DEF(I,2)=1
					    S_DEF(I,3)=DEF
				    ELSE IF (VERIFY=="Y".OR.VERIFY=="y") THEN
					    WRITE (1,100) "DEFLECTION (RAD)? (PAY ATTENTION TO THE POSITIVES AND NEGATIVES) "
					    READ (1,*) DEF
					    DISP(3*N)=DEF
					    DOF(3*N)=0
					    S_DEF(I,1)=N
					    S_DEF(I,2)=0
					    S_DEF(I,3)=DEF
				    ELSE
					    WRITE (1,101) "YOU MUST HAVE MADE A TYPING MISTAKE. TRY AGAIN."
					    GOTO 30	
				    END IF
		    END DO
	    ELSE IF (VERIFY=="N".OR.VERIFY=="n") THEN
		    GOTO 55
	    ELSE
		    WRITE (1,101), "ARE YOU KIDDING ME? PAY MORE ATTENTION TO WHAT YOU TYPE!"
		    GOTO 29		
	    END IF	
28	    WRITE (1,104) "DO YOU VERIFY ALL THE DATA YOU JUST ENTERED? <Y/N> : "
	    VERIFY= GETCHARQQ ( ) 
	    IF (VERIFY=="Y".OR.VERIFY=="y") THEN
		    GOTO 55
	    ELSE IF (VERIFY=="N".OR.VERIFY=="n") THEN
		    DOF=DOFBACKUP
		    FEM=FEMBACKUP
		    DISP=DISPBACKUP
		    DEALLOCATE (S_DEF)
		    GOTO 29
	    ELSE
		    GOTO 28
	    END IF
55	    call clearscreen($gclearscreen)
	    DISPBACKUP=DISP
	    !SORTING OF S_DEF
		DO II=1,NUM_DEF
			MAXX=II
			DO JJ=II+1,NUM_DEF
				IF(S_DEF(MAXX,1)<S_DEF(JJ,1)) THEN
					MAXX=JJ
					SWAPTEMP=S_DEF(MAXX,1)
					S_DEF(MAXX,1)=S_DEF(II,1)
					S_DEF(II,1)=SWAPTEMP

					SWAPTEMP=S_DEF(MAXX,2)
					S_DEF(MAXX,2)=S_DEF(II,2)
					S_DEF(II,2)=SWAPTEMP

					SWAPTEMP=S_DEF(MAXX,3)
					S_DEF(MAXX,3)=S_DEF(II,3)
					S_DEF(II,3)=SWAPTEMP
				END IF
			END DO
		END DO
						
!********************************************************************************************************************************************
!BUILD ERROR---------------------------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************
41	    WRITE (1,102) "BUILD ERROR", "-----------"
	    WRITE (1,104) "ARE THERE ANY BUILD ERRORS? <Y/N> "
	    VERIFY= GETCHARQQ ( ) 
	    IF (VERIFY=="Y".OR.VERIFY=="y") THEN
		    WRITE (1,104) "ENTER THE NUMBER OF ELEMENTS WITH A BUILD ERROR: "
		    READ (1,*) NUM_BLD
		    DO I=1,NUM_BLD
			    WRITE(1,104) "THE NUMBER OF THE ELEMENT? "
			    READ(1,*) N
				    WRITE (1,100) "BUILD ERROR (mm)? (PAY ATTENTION TO POSITIVES AND NEGATIVES) "
				    READ (1,*) BLD
				    L=ELEMENT(N,6)
				    FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11))+ELEMENT(N,8)*ELEMENT(N,10)*BLD/L				!FIRST NODE FX
				    FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)+0								!FIRST NODE FY
				    FEMTEMP(3*ELEMENT(N,11))=FEMTEMP(3*ELEMENT(N,11))+0								!FIRST NODE MOMENT	
				    FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-2)-ELEMENT(N,8)*ELEMENT(N,10)*BLD/L				!SECOND NODE FX
				    FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)+0								!SECOND NODE FY
				    FEMTEMP(3*ELEMENT(N,12))=FEMTEMP(3*ELEMENT(N,12))+0								!SECOND NODE MOMENT	
				    !CONVERSION TO THE GLOBAL SYSTEM
				    FEMTEMP(3*ELEMENT(N,11)-2)=FEMTEMP(3*ELEMENT(N,11)-1)*ELEMENT(N,18)+FEMTEMP(3*ELEMENT(N,11)-2)*ELEMENT(N,17)
				    FEMTEMP(3*ELEMENT(N,11)-1)=FEMTEMP(3*ELEMENT(N,11)-1)*ELEMENT(N,17)-FEMTEMP(3*ELEMENT(N,11)-2)*ELEMENT(N,18)
				    FEMTEMP(3*ELEMENT(N,12)-2)=FEMTEMP(3*ELEMENT(N,12)-1)*ELEMENT(N,18)+FEMTEMP(3*ELEMENT(N,12)-2)*ELEMENT(N,17)
				    FEMTEMP(3*ELEMENT(N,12)-1)=FEMTEMP(3*ELEMENT(N,12)-1)*ELEMENT(N,17)-FEMTEMP(3*ELEMENT(N,12)-2)*ELEMENT(N,18)
				    !ADDITION TO OTHER FEMs
				    FEM(3*ELEMENT(N,11)-2)=FEM(3*ELEMENT(N,11)-2)+FEMTEMP(3*ELEMENT(N,11)-2)			!FIRST NODE FX
				    FEM(3*ELEMENT(N,11)-1)=FEM(3*ELEMENT(N,11)-1)+FEMTEMP(3*ELEMENT(N,11)-1)			!FIRST NODE FY
				    FEM(3*ELEMENT(N,11))=FEM(3*ELEMENT(N,11))+FEMTEMP(3*ELEMENT(N,11))				!FIRST NODE MOMENT	
				    FEM(3*ELEMENT(N,12)-2)=FEM(3*ELEMENT(N,12)-2)+FEMTEMP(3*ELEMENT(N,12)-2)			!SECOND NODE FX
				    FEM(3*ELEMENT(N,12)-1)=FEM(3*ELEMENT(N,12)-1)+FEMTEMP(3*ELEMENT(N,12)-1)			!SECOND NODE FY
				    FEM(3*ELEMENT(N,12))=FEM(3*ELEMENT(N,12))+FEMTEMP(3*ELEMENT(N,12))				!SECOND NODE MOMENT
				    FEM_EL(1:6,N)=FEM_EL(1:6,N)+FEMTEMP(3*ELEMENT(N,11)-2:3*ELEMENT(N,12))          !NEWWWWWW
				    FEMTEMP=0													
		    END DO
	    ELSE IF (VERIFY=="N".OR.VERIFY=="n") THEN
		GOTO 60
	    ELSE
		    WRITE (1,101), "ARE YOU KIDDING ME? PAY MORE ATTENTION TO WHAT YOU TYPE!"
		    GOTO 41
	    END IF
40	    WRITE (1,104) "DO YOU VERIFY ALL THE DATA YOU JUST ENTERED? <Y/N> : "
	    VERIFY= GETCHARQQ ( ) 
	    IF (VERIFY=="Y".OR.VERIFY=="y") THEN
		    GOTO 60
	    ELSE IF (VERIFY=="N".OR.VERIFY=="n") THEN
		    FEM=FEMBACKUP
		    GOTO 41
	    ELSE
		    GOTO 40
	    END IF
60	    call clearscreen($gclearscreen)

	    FEMBACKUP=FEM

!********************************************************************************************************************************************
!CALCULATIONS2--------------------------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************
	    KF=0
	    FEMF=0
	    KFS=0
	    DELTAF=0
	    DELTAS=0
												!FORMATION OF FREE STIFFNESS MATRIX AND FREE NODES FORCES
	    XX=1
	    DO I=1,3*NUM_NODE
		    IF(DOF(I)==1) THEN
			    YY=1
				    DO J=1,3*NUM_NODE
					    IF(DOF(J)==1) THEN
						    KF(XX,YY)=K(I,J)
						    YY=YY+1
				    	END IF
			    	END DO
			    FEMF(XX)=FEM(I)
			    XX=XX+1
		    END IF
	    END DO
							!FORMATION OF FREE-SECURED STIFFNESS MATRIX
	    XX=1
	    DO I=1,3*NUM_NODE
		    IF(DOF(I)==1) THEN
			    YY=1
				DO J=1,3*NUM_NODE
					IF(DOF(J)/=1) THEN
						KFS(XX,YY)=K(I,J)
						YY=YY+1
					END IF
				END DO
			XX=XX+1
		    END IF
	    END DO
							!ASSEMBLAGE OF DELTA S
	    XX=1
	    DO I=1,3*NUM_NODE
		    DO J=1,NUM_DEF
			    IF(INT((3*S_DEF(J,1)-S_DEF(J,2)))==I) THEN	
				    DELTAS(XX)=S_DEF(J,3)
				    XX=XX+1
			    ELSE IF(EOF(I)==0) THEN
				    XX=XX+1
			    END IF
		    END DO
	    END DO
        CALL INVERSE(KF,KFINV,COUNT)							!INVERSE OF FREE STIFFNESS MATRIX 
	    DELTAF=MATMUL(KFINV,((-1)*FEMF-MATMUL(KFS,DELTAS)))	!SOLVING THE EQUATION FOR DELTA F

	    !ASSEMBLAGE OF GLOBAL DISPLACEMENT MATRIX (DELTA)
	    COUNTS=1
	    COUNTF=1
	    DO II=1,3*NUM_NODE
		    IF(DOF(II)==0) THEN
		    	DISP(II)=DELTAS(COUNTS)
			    COUNTS=COUNTS+1
		    ELSE IF(DOF(II)==1) THEN
			    DISP(II)=DELTAF(COUNTF)
			    COUNTF=COUNTF+1
		    END IF
	    END DO
	    !THE FINAL ANSWERS
	    FORCE=MATMUL(K,DISP)+FEM	
		
!********************************************************************************************************************************************
!OUTPUT--------------------------------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************
	    !NEWWWWWWWWWWWWWW(ELEMENT FORCES)
	    WRITE (1,102) "OUTPUT", "------"
	    OPEN(3,FILE="OUTPUT.TXT")						!THE OUTPUT.TXT FILE
	    WRITE (3,102) "OUTPUT", "------"
	    WRITE (1,103) "ELEMENT FORCES IN GLOBAL CO-ORDINATE SYSTEM ARE AS FOLLOWS: "
	    WRITE (3,103) "ELEMENT FORCES IN GLOBAL CO-ORDINATE SYSTEM ARE AS FOLLOWS: "
	    WRITE (1,*) 
	    WRITE (3,*)
    
        DO I=1,NUM_ELEM
	        F_EL=0
		    KG=0
		    SCU=0
            DELTA_EL=0
		    CALL KGELOBAL(ELEMENT_BACKUP(I,8),ELEMENT_BACKUP(I,9),ELEMENT_BACKUP(I,10),ELEMENT_BACKUP(I,6)*1000,COS(ELEMENT_BACKUP(I,7)),SIN(ELEMENT_BACKUP(I,7)),KG)
	        DELTA_EL(1:3)=DISP(3*ELEMENT_BACKUP(I,11)-2:3*ELEMENT_BACKUP(I,11))
	        DELTA_EL(4:6)=DISP(3*ELEMENT_BACKUP(I,12)-2:3*ELEMENT_BACKUP(I,12))
		    SCU(1:6)=FEM_EL(1:6,I)
		    F_EL=MATMUL(KG,DELTA_EL)+SCU
	        WRITE (1,106) "   ELEM NUM.", "   X-AXIS FORCE (N)", "   Y-AXIS FORCE (N)", "    MOMENT (N.mm)"
	        WRITE (3,106) "   ELEM NUM.", "   X-AXIS FORCE (N)", "   Y-AXIS FORCE (N)", "    MOMENT (N.mm)"
	        WRITE (1,101) "-------------------------------------------------------------------------------"
	        WRITE (3,101) "-------------------------------------------------------------------------------"
	        WRITE (1,*) I
	        WRITE (3,*) I
    	    WRITE (1,117)  F_EL(1:3)
		    WRITE (3,117)  F_EL(1:3)
		    WRITE (1,117)  F_EL(4:6)
		    WRITE (3,117)  F_EL(4:6)
	    END DO
	    WRITE (1,*)
	    WRITE (3,*)
	    WRITE (1,100) "PRESS ANY KEY TO PROCEED "
	    VERIFY= GETCHARQQ ( ) 
	    call clearscreen($gclearscreen)

	    !NODAL DISPLACEMENTS
	    WRITE (1,102) "OUTPUT", "------"
	    WRITE (1,103) "NODAL DISPLACEMENTS IN GLOBAL CO-ORDINATE SYSTEM ARE AS FOLLOWS: "
	    WRITE (3,103) "NODAL DISPLACEMENTS IN GLOBAL CO-ORDINATE SYSTEM ARE AS FOLLOWS: "
	    WRITE (1,*) 
	    WRITE (3,*) 
	    WRITE (1,106) "NODE NUM.", "X DISP. (mm)", "Y DISP. (mm)", "   ROTATION (RAD)"
	    WRITE (3,106) "NODE NUM.", "X DISP. (mm)", "Y DISP. (mm)", "   ROTATION (RAD)"
	    WRITE (1,101) "---------------------------------------------------------------------------"
	    WRITE (3,101) "---------------------------------------------------------------------------"
	    DO I=1,NUM_NODE
		    WRITE (1,115) I, DISP(3*I-2), DISP(3*I-1), DISP(3*I-0)
		    WRITE (3,115) I, DISP(3*I-2), DISP(3*I-1), DISP(3*I-0)
	    END DO
	    WRITE (3,101) "---------------------------------------------------------------------------"
	    CLOSE(3)
	    WRITE (1,103) "AN APROXIMATE DISPLACED FRAME IS DRAWN IN THE WINDOOW TO THE RIGHT."
	    WRITE (1,101) "THE DEFLECTIONS IN THE DRAWING ARE MULTIPLIED BY 10."

	    !NEW NODE CO-ORDINATES
	    DO I=1,NUM_NODE
		    NODE(I,1)=NODE(I,1)+0.1*DISP(3*I-2)
		    NODE(I,2)=NODE(I,2)+0.1*DISP(3*I-1)
	    END DO

	    !LINEAR DRAWING OF THE DISPLACED FRAME
	    DO II=1,NUM_ELEM
		    J=FOCUSQQ(2)												!SELECTS WINDOW NUMBER 2 (GRAPHICS)
		    I=SETCOLOR (2442)											!SETS GRAPH COLOR TO GREY
		    L=LINE(NODE(ELEMENT(II,11),1),NODE(ELEMENT(II,11),2),NODE(ELEMENT(II,12),1),NODE(ELEMENT(II,12),2))
		    J=FOCUSQQ(1)												!BACK TO SCREEN NUMBER 1 (DATA)
    	END DO

	    WRITE (1,*)
	    WRITE (1,100) "PRESS ANY KEY TO PROCEED "
	    VERIFY= GETCHARQQ ( ) 
98	    call clearscreen($gclearscreen)

!TRUSS---------------------------------------------------------------------------------------------------------------------------------------
    CASE("T")                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
!nodes---------------------------------------------------------------------------------------------------------------------------------------                                                                                                                                                                                                                                                                                                                                                                       
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
200	    write (1,102) "Nodes","-----"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
	    write (1,104) "Enter the number of nodes: "                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
	    read (1,*),num_node								!total number of nodes                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
	    allocate (node(num_node,2),delta_el(4),F_EL(4),scu(4),force(2*num_node),disp(2*num_node),dof(2*num_node),fem(2*num_node),femtemp(2*num_node))                                                                                                                                                                                                                                                                                                                                                     
	    allocate (kg(4,4),k(2*num_node,2*num_node))                                                                                                                                                                                                                                                                                                                                                                                                                                                         
	    k=0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
	    kg=0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
	    t=0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
	    node=0; force=0; disp=0; fem=0; femtemp=0                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
	    dof=1								!so that the degrees of freedom not be zero by default  (All nodes are assumed to be free)                                                                                                                                                                                                                                                                                                                                                                                                                                              
	    do i=1,num_node                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
		    write (1,103) "Node number", i,":"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
		    write (3,103) "Node number", i,":"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
		    write (1,100) "X co-ordinate: (m) "                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
		    write (3,100) "X co-ordinate: (m) "                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
		    read (1,*) node(i,1)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
		    write (3,118) node(i,1)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
		    write (1,100) "Y co-ordinate: (m) "                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
		    write (3,100) "Y co-ordinate: (m) "                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
		    read (1,*) node(i,2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
		    write (3,118) node(i,2) 
		    J=FOCUSQQ(2)								!SELECTS WINDOW NUMBER 2 (GRAPHICS)
		    P=SETPIXEL_W(NODE(I,1),NODE(I,2))			!DRAWS THE NODE		
		    J=FOCUSQQ(1)								!BACK TO SCREEN NUMBER 1 (DATA)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
	    end do                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
215	    WRITE (1,104) "DO YOU VERIFY ALL THE DISPLAYED NODE CO-ORDINATES? <Y/N> : "
	    VERIFY= GETCHARQQ ( ) 
	    IF (VERIFY=="Y".OR.VERIFY=="y") THEN
		    GOTO 213
	    ELSE IF (VERIFY=="N".OR.VERIFY=="n") THEN
		    call clearscreen($gclearscreen)
		    DEALLOCATE (NODE,DELTA_EL,F_EL,SCU,LOAD,FORCE,DISP,DOF,DOFBACKUP,FEM,FEMTEMP,FEMBACKUP,DISPBACKUP)
		    DEALLOCATE (KL,KG,K,T,LOCAL)
		    GOTO 200
	    ELSE
		    GOTO 215
	    END IF
213	    call clearscreen($gclearscreen)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
!elements------------------------------------------------------------------------------------------------------------------------------------                                                                                                                                                                                                                                                                                                                                                                       
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
211	    write (1,102) "Elements", "--------"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
	    write (1,104) "Enter the number of elements: "                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
	    read (1,*) num_elem																		!total number of elements                                                                                                                                                                                                                                                                                                                                                                                                                                                     
	    allocate (element(num_elem,18),element_backup(num_elem,18))                                                                                                                                                                                                                                                                                                                                                                                                                                     
	    element=0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
	    do i=1,num_elem                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
		    element(i,1)=i																		!the number of element                                                                                                                                                                                                                                                                                                                                                                                                                                                            
		    write (1,105)"Element number",i,":"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
		    write (1,100)"The number of node at the first point of member : "                                                                                                                                                                                                                                                                                                                                                                                                                                                 
		    read (1,*) element (i,11)															!node1                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
		    write (1,100)"The number of node at the second point of member: "                                                                                                                                                                                                                                                                                                                                                                                                                                                 
		    read (1,*) element (i,12)															!node2                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
		    element(i,2)=node(element (i,11),1)													!x1                                                                                                                                                                                                                                                                                                                                                                                                                                                               
		    element(i,3)=node(element (i,11),2)													!y1	                                                                                                                                                                                                                                                                                                                                                                                                                                                              
		    element(i,4)=node(element (i,12),1)													!x2                                                                                                                                                                                                                                                                                                                                                                                                                                                               
		    element(i,5)=node(element (i,12),2)													!y2                                                                                                                                                                                                                                                                                                                                                                                                                                                               
		    element(i,6)=sqrt((element(i,3)-element(i,5))**2+(element(i,2)-element(i,4))**2)	!lenght of member																	                                                                                                                                                                                                                                                                                                                                                                                               
		    element(i,7)=atan((element(i,5)-element(i,3))/(element(i,4)-element(i,2)))			!theta (rad)																                                                                                                                                                                                                                                                                                                                                                                                                         
		    element (i,13)=node(element (i,11),1)												!node 1 x                                                                                                                                                                                                                                                                                                                                                                                                                                                        
		    element (i,14)=node(element (i,11),2)												!node 1 y                                                                                                                                                                                                                                                                                                                                                                                                                                                        
		    element (i,15)=node(element (i,12),1)												!node 2 x                                                                                                                                                                                                                                                                                                                                                                                                                                                        
		    element (i,16)=node(element (i,12),2)												!node 2 y                                                                                                                                                                                                                                                                                                                                                                                                                                                        
		    element (i,17)=(element(i,4)-element(i,2))/element(i,6)								!cos thetha		                                                                                                                                                                                                                                                                                                                                                                                                                                    
		    element (i,18)=(element(i,5)-element(i,3))/element(i,6)								!sin thetha      
		        														!DRAWING THE ELEMENTS
		    J=FOCUSQQ(2)												!SELECTS WINDOW NUMBER 2 (GRAPHICS)
		    L=LINE(ELEMENT(I,2),ELEMENT(I,3),ELEMENT(I,4),ELEMENT(I,5))
		    J=FOCUSQQ(1)												!BACK TO SCREEN NUMBER 1 (DATA)                                                                                                                                                                                                                                                                                                                                                                                                                                
	    end do                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
        WRITE (1,103) "THE ELEMENTS OF THE STRUCTURE ARE DRAWN IN THE WINDOW TO THE RIGHT."
212	    WRITE (1,100) "DO YOU VERIFY ALL THE DISPLAYED MEMBER INFORMATION? <Y/N> : "
	    VERIFY= GETCHARQQ ( ) 
	    IF (VERIFY=="Y".OR.VERIFY=="y") THEN
		    GOTO 210
	    ELSE IF (VERIFY=="N".OR.VERIFY=="n") THEN
		    call clearscreen($gclearscreen)
		    J=FOCUSQQ(2)								!SELECTS WINDOW NUMBER 2 (GRAPHICS)
		    call clearscreen($gclearscreen)
		    L=LENGHT
		    H=HEIGHT
		    CALL SETLINESTYLE(#F0F0)						!SET LINE STYLE TO DOTTED LINE
		    I=SETCOLOR (8)									!SETS GRIDLINE COLOR TO "GREY"
		    G=GRID(2*(L+1),2*(H+1))							!DRAWS GRIDLINES
		    CALL SETLINESTYLE(#FFFF)						!SET LINE STYLE TO SIMPLE LINE
		    I=SETCOLOR (4)					
		    J=FOCUSQQ(1)								!BACK TO SCREEN NUMBER 1 (DATA)
		    DEALLOCATE(ELEMENT)
		    GOTO 211
	    ELSE
		    GOTO 212
	    END IF
210	    call clearscreen($gclearscreen)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
!elements area-------------------------------------------------------------------------------------------------------------------------------                                                                                                                                                                                                                                                                                                                                                                       
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
	    write (1,102) "Section properties", "------------------", "Cross-section area"				!section properties                                                                                                                                                                                                                                                                                                                                                                                                              
	    num=num_elem                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
	    do i=1,num                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
		    write (1,103) "Element number", i,":"                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
		    number=i                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
		    write (1,100) "Cross-section area? (mm^2) "                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
		    read (1,*) area                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
		    element(number,10)=area                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
	    end do                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
       call clearscreen($gclearscreen)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
!elements modulus of elasticity--------------------------------------------------------------------------------------------------------------                                                                                                                                                                                                                                                                                                                                                                       
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
	    write (1,102) "Section properties", "------------------", "Modulus of elasticity"				!section properties                                                                                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
	    num=num_elem											!members with a different modulus of elasticity                                                                                                                                                                                                                                                                                                                                                                                                                                             
	    do i=1,num                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
		    write (1,103) "Element number", i,":"                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
		    number=i                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
		    write (1,100) "Eodulus of elasticity? (mpa) "                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
		    read (1,*) e                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
		    element(number,8)=e                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
	    end do                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
        call clearscreen($gclearscreen)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
!section properties output-------------------------------------------------------------------------------------------------------------------                                                                                                                                                                                                                                                                                                                                                                       
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
	    write (3,103) "The members section properties are as follows:"								!section properties verification                                                                                                                                                                                                                                                                                                                                                                                                             
	    write (3,106) "no.","E (mpa)","A (mm^4)"                                                                                                                                                                                                                                                                                                                                                                                                                                                            
	    write (3,101) "---------------------------------------------------------------------------"                                                                                                                                                                                                                                                                                                                                                                                                                        
	    do i=1,num_elem                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
		    write (3,107) element(i,1), element(i,8) ,element(i,10)                                                                                                                                                                                                                                                                                                                                                                                                                                                             
	    end do                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
	    write (3,*)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
        call clearscreen($gclearscreen)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
!supports------------------------------------------------------------------------------------------------------------------------------------                                                                                                                                                                                                                                                                                                                                                                       
	                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
317	    write (1,102) "Supports", "--------"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
	    write (1,103) "Support types:"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
	    write (1,101) "1: pin support"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
	    write (1,101) "2: roller support"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
	    write (1,101) "----------------------------------------"                                                                                                                                                                                                                                                                                                                                                                                                                                                           
	    write (1,104) "Enter the number of supported nodes: "                                                                                                                                                                                                                                                                                                                                                                                                                                                              
	    read (1,*) num_sup                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
	    do i=1,num_sup                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
		    write (1,104) "The number of node: "                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
		    read (1,*) n                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
188		    write (1,100) "Type of support:"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
		    read (1,*) support                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
		    select case (support)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
			    case (1)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
				    dof(2*n-1)=0								!x dof                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
				    dof(2*n)=0									!y dof 
				    J=FOCUSQQ(2)								!SELECTS WINDOW NUMBER 2 (GRAPHICS)
				    L=LENGHT
				    H=HEIGHT
				    SUPPORT=PIN(NODE(N,1),NODE(N,2),L,H)		!DRAWS THE SUPPORT
				    J=FOCUSQQ(1)								!BACK TO SCREEN NUMBER 1 (DATA)	                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
			    case (2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
131				    write (1,100) "Vertical or horizontal? (v/h)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
				    read (1,*) verify                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
				    if (verify=="v".or.verify=="V") then                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
					    dof(2*n-1)=0								!x dof                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
					    dof(2*n)=1									!y dof  
					    J=FOCUSQQ(2)								!SELECTS WINDOW NUMBER 2 (GRAPHICS)
					    L=LENGHT
					    H=HEIGHT
					    SUPPORT=ROLLERV(NODE(N,1),NODE(N,2),L,H)	!DRAWS THE SUPPORT
					    J=FOCUSQQ(1)								!BACK TO SCREEN NUMBER 1 (DATA)	                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
				    else if (verify=="h".or.verify=="H") then                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
					    dof(2*n-1)=1								!x dof                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
					    dof(2*n)=0									!y dof  
					    J=FOCUSQQ(2)								!SELECTS WINDOW NUMBER 2 (GRAPHICS)
					    L=LENGHT
					    H=HEIGHT
					    SUPPORT=ROLLERV(NODE(N,1),NODE(N,2),L,H)	!DRAWS THE SUPPORT
					    J=FOCUSQQ(1)								!BACK TO SCREEN NUMBER 1 (DATA)	                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
				    else                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
					    write(*,101), "Typing mistake mate! Try again."                                                                                                                                                                                                                                                                                                                                                                                                                                                                
					    goto 131		                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
				    end if                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          	                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
		    case default                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
			    write (1,*) "Typing error! Try again!"                                                                                                                                                                                                                                                                                                                                                                                                                                           
			    goto 188                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
		    end select                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
	    end do                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
218	    WRITE (1,104) "DO YOU VERIFY ALL THE DISPLAYED SUPPORTS <Y/N> : "
	    VERIFY= GETCHARQQ ( ) 
	    IF (VERIFY=="Y".OR.VERIFY=="y") THEN
		    GOTO 216
	    ELSE IF (VERIFY=="N".OR.VERIFY=="n") THEN
		    J=FOCUSQQ(2)
		    call clearscreen($gclearscreen)
		    L=LENGHT
		    H=HEIGHT
		    CALL SETLINESTYLE(#F0F0)						!SET LINE STYLE TO DOTTED LINE
		    I=SETCOLOR (8)									!SETS GRIDLINE COLOR TO "GREY"
		    G=GRID(2*(L+1),2*(H+1))							!DRAWS GRIDLINES
		    CALL SETLINESTYLE(#FFFF)						!SET LINE STYLE TO SIMPLE LINE
		    I=SETCOLOR (4)									!SETS GRAPH COLOR TO "RED"
		    DO I=1,NUM_ELEM
			    L=LINE(ELEMENT(I,2),ELEMENT(I,3),ELEMENT(I,4),ELEMENT(I,5))
	    	END DO
		    J=FOCUSQQ(1)
		    call clearscreen($gclearscreen)
		    DOF=DOFBACKUP
		    GOTO 317
    	ELSE
	    	GOTO 218
	    END IF
216	    call clearscreen($gclearscreen)
	    DOFBACKUP=DOF
	    FEM=0	
	    FEMTEMP=0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
!calculations1--------------------------------------------------------------------------------------------------------------------------------                                                                                                                                                                                                                                                                                                                                                                      
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
        element_backup=element	                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
	    k=0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
	    do i=1,num_elem                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
		    kg=0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
	    !call kglobal(element(i,8),element(i,10),element(i,6)*1000,cos(element(i,7)),sin(element(i,7)),kg)
	        eee=element(i,8)
	        aaa=element(i,10)
	        lll=element(i,6)
	        ccc=cos(element(i,7))
	        sss=sin(element(i,7))

	        ttt=eee*aaa/lll                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
	        kg(1,1)=ttt*ccc*ccc
	        kg(1,2)=ttt*ccc*sss
	        kg(1,3)=ttt*ccc*ccc*(-1)
	        kg(1,4)=ttt*ccc*sss*(-1)
	        kg(2,1)=ttt*ccc*sss
	        kg(2,2)=ttt*sss*sss
	        kg(2,3)=ttt*ccc*sss*(-1)
	        kg(2,4)=ttt*sss*sss*(-1)
	        kg(3,1)=ttt*ccc*ccc*(-1)
	        kg(3,2)=ttt*ccc*sss*(-1)
	        kg(3,3)=ttt*ccc*ccc
	        kg(3,4)=ttt*ccc*sss
	        kg(4,1)=ttt*ccc*sss*(-1)
	        kg(4,2)=ttt*sss*sss*(-1)
	        kg(4,3)=ttt*ccc*sss
	        kg(4,4)=ttt*sss*sss                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
		    !assemblage of global stiffness matrix                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
		    !first column                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
		    k(2*element(i,11)-1,2*element(i,11)-1)=k(2*element(i,11)-1,2*element(i,11)-1)+kg(1,1)                                                                                                                                                                                                                                                                                                                                                                                                                             
		    k(2*element(i,11)-0,2*element(i,11)-1)=k(2*element(i,11)-0,2*element(i,11)-1)+kg(2,1)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
		    k(2*element(i,12)-1,2*element(i,11)-1)=k(2*element(i,12)-1,2*element(i,11)-1)+kg(3,1)                                                                                                                                                                                                                                                                                                                                                                                                                             
		    k(2*element(i,12)-0,2*element(i,11)-1)=k(2*element(i,12)-0,2*element(i,11)-1)+kg(4,1)                                                                                                                                                                                                                                                                                                                                                                                                                             
                                                                                                                                                                                                                                                                                                                                                                                                                           
		    !second column                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
		    k(2*element(i,11)-1,2*element(i,11)-0)=k(2*element(i,11)-1,2*element(i,11)-0)+kg(1,2)                                                                                                                                                                                                                                                                                                                                                                                                                             
	    	k(2*element(i,11)-0,2*element(i,11)-0)=k(2*element(i,11)-0,2*element(i,11)-0)+kg(2,2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
		    k(2*element(i,12)-1,2*element(i,11)-0)=k(2*element(i,12)-1,2*element(i,11)-0)+kg(3,2)                                                                                                                                                                                                                                                                                                                                                                                                                             
		    k(2*element(i,12)-0,2*element(i,11)-0)=k(2*element(i,12)-0,2*element(i,11)-0)+kg(4,2)                                                                                                                                                                                                                                                                                                                                                                                                                             
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
		    !third column                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
		    k(2*element(i,11)-1,2*element(i,12)-1)=k(2*element(i,11)-1,2*element(i,12)-1)+kg(1,3)                                                                                                                                                                                                                                                                                                                                                                                                                             
		    k(2*element(i,11)-0,2*element(i,12)-1)=k(2*element(i,11)-0,2*element(i,12)-1)+kg(2,3)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
	    	k(2*element(i,12)-1,2*element(i,12)-1)=k(2*element(i,12)-1,2*element(i,12)-1)+kg(3,3)                                                                                                                                                                                                                                                                                                                                                                                                                             
	    	k(2*element(i,12)-0,2*element(i,12)-1)=k(2*element(i,12)-0,2*element(i,12)-1)+kg(4,3)                                                                                                                                                                                                                                                                                                                                                                                                                             
                                                                                                                                                                                                                                                                                                                                                                                                                            
		    !fourth column                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
		    k(2*element(i,11)-1,2*element(i,12)-0)=k(2*element(i,11)-1,2*element(i,12)-0)+kg(1,4)                                                                                                                                                                                                                                                                                                                                                                                                                             
		    k(2*element(i,11)-0,2*element(i,12)-0)=k(2*element(i,11)-0,2*element(i,12)-0)+kg(2,4)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
		    k(2*element(i,12)-1,2*element(i,12)-0)=k(2*element(i,12)-1,2*element(i,12)-0)+kg(3,4)                                                                                                                                                                                                                                                                                                                                                                                                                             
		    k(2*element(i,12)-0,2*element(i,12)-0)=k(2*element(i,12)-0,2*element(i,12)-0)+kg(4,4)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                                                                                                                                                                                            
	    end do                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
	    count=0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
	    do i=1, 2*num_node                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
		    if(dof(i)==1) count=count+1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
	    end do                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
	    allocate(kf(count,count), femf(count),deltaf(count),deltas(2*num_node-count),kfinv(count,count))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
!loading-------------------------------------------------------------------------------------------------------------------------------------                                                                                                                                                                                                                                                                                                                                                                       
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
222	    write (1,102) "Loading", "-------"	                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
	    write (1,103) "-The Principle of Superposition may be required-"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
	    write (1,103) "Load cases:"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
	    write (1,103) "1: concentrated perpendicular load (In Global Co-ordinates)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
	    write (1,101) "2: concentrated tangential load    (In Global Co-ordinates)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
	    write (1,101) "----------------------------------------"                                                                                                                                                                                                                                                                                                                                                                                                                                                           
	    write (1,104) "Enter the number of loads:"                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
	    read (1,*)num_load                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
	    do i=1,num_load                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
		    write (1,104) "The number of node: "                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
		    read (1,*) n                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
119		    write (1,100)"Load case: "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
		    read (1,*) ld                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
		    select case (ld)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
			    case (1)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
				    write(1,100) "Load magnitude? (n) "                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
				    read(1,*) p                                                                                                                                                                                                                                                                                                                                                                                                                     
				    fem(2*n-0)=fem(2*n-0)+p			!fy                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
				    femtemp=0					                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  					                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
			    case (2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
				    write(1,100) "Load magnitude? (n) "                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
				    read(1,*) p                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
				    fem(2*n-1)=fem(2*n-1)+p			!fx                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
				    femtemp=0					                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  									                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
			    case default                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
				    write (1,*) "Typing error! Try again!"                                                                                                                                                                                                                                                                                                                                                                                                                                          
				    goto 119                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
		    end select                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
	    end do                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
       
223	    WRITE (1,104) "DO YOU VERIFY ALL THE LOADING DATA YOU JUST ENTERED? <Y/N> : "
	    VERIFY= GETCHARQQ ( ) 
	    IF (VERIFY=="Y".OR.VERIFY=="y") THEN
		    GOTO 221
	    ELSE IF (VERIFY=="N".OR.VERIFY=="n") THEN
		    call clearscreen($gclearscreen)
		    !J=FOCUSQQ(2)
		    !call clearscreen($gclearscreen)
		    !L=LENGHT
		    !H=HEIGHT
		    !CALL SETLINESTYLE(#F0F0)						!SET LINE STYLE TO DOTTED LINE
		    !I=SETCOLOR (8)									!SETS GRIDLINE COLOR TO "GREY"
		    !G=GRID(2*(L+1),2*(H+1))							!DRAWS GRIDLINES
		    !CALL SETLINESTYLE(#FFFF)						!SET LINE STYLE TO SIMPLE LINE
		    !I=SETCOLOR (4)									!SETS GRAPH COLOR TO "RED"
		    !DO I=1,NUM_ELEM
			    !L=LINE(ELEMENT(I,2),ELEMENT(I,3),ELEMENT(I,4),ELEMENT(I,5))
		    !END DO
		    !J=FOCUSQQ(1)
		    FEM=0
		    call clearscreen($gclearscreen)
		    GOTO 222
	    ELSE
		    GOTO 223
	    END IF
221	    call clearscreen($gclearscreen)
	    FEMBACKUP=FEM
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
!calculations2--------------------------------------------------------------------------------------------------------------------------------                                                                                                                                                                                                                                                                                                                                                                      
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
        kf=0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
	    femf=0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
	    deltaf=0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
	    deltas=0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
	    !formation of free stiffness matrix and free nodes forces                                                                                                                                                                                                                                                                                                                                                                                                                                               
	    xx=1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
	    do i=1,2*num_node                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
		    if(dof(i)==1) then                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
			    yy=1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
				do j=1,2*num_node                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
					if(dof(j)==1) then                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
						kf(xx,yy)=k(i,j)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
						yy=yy+1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
					end if                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
				end do                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
			femf(xx)=fem(i)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
			xx=xx+1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
		    end if                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
	    end do   
	                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
	    call inverse(kf,kfinv,count)!inverse of free stiffness matrix                                                                                                                                                                                                                                                                                                                                                                                                                                                
	    deltaf=matmul(kfinv,femf)	!solving the equation for delta f                                                                                                                                                                                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
	    !assemblage of global displacement matrix (delta)                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
	    counts=1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
	    countf=1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
	    do ii=1,2*num_node                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
		    if(dof(ii)==0) then                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
			    disp(ii)=deltas(counts)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
			    counts=counts+1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
		    else if(dof(ii)==1) then                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
			    disp(ii)=deltaf(countf)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
			    countf=countf+1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
		    end if                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
	    end do                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
	    !the final answers                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
	    force=matmul(k,disp)+  fem	                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
!output--------------------------------------------------------------------------------------------------------------------------------------                                                                                                                                                                                                                                                                                                                                                                       
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
	!newwwwwwwwwwwwww(element forces)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
	    write (1,102) "Output - Forces", "-------------------"                                                                                                                                                                                                                                                                                                                                                                                                                                         
	    write (3,102) "Output", "------"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
	    write (1,103) "Element forces in global co-ordinate system are as follows: "                                                                                                                                                                                                                                                                                                                                                                                                                                       
	    write (3,103) "Element forces in global co-ordinate system are as follows: "                                                                                                                                                                                                                                                                                                                                                                                                                                       
	    write (1,*)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
	    write (3,*)  
	    write (1,106) "   elem num.", "   axial force (n)", "   compression/tension"                                                                                                                                                                                                                                                                                                                                                                                                           
	    write (3,106) "   elem num.", "   axial force (n)", "   compression/tension"                                                                                                                                                                                                                                                                                                                                                                                                                
	    write (1,101) "----------------------------------------------------------"                                                                                                                                                                                                                                                                                                                                                                                                                    
	    write (3,101) "----------------------------------------------------------"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
        do i=1,num_elem                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
	        f_el=0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
		    kg=0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
		    scu=0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
            delta_el=0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
		    !call kglobal(element_backup(i,8),element_backup(i,10),element_backup(i,6)*1000,cos(element_backup(i,7)),sin(element_backup(i,7)),kg)                                                                                                                                                                                                                                                                                                                                                         
	    eee=element(i,8)
	    aaa=element(i,10)
	    lll=element(i,6)
	    ccc=cos(element(i,7))
	    sss=sin(element(i,7))

	    ttt=eee*aaa/lll                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
	    kg(1,1)=ttt*ccc*ccc
	    kg(1,2)=ttt*ccc*sss
	    kg(1,3)=ttt*ccc*ccc*(-1)
	    kg(1,4)=ttt*ccc*sss*(-1)
	    kg(2,1)=ttt*ccc*sss
	    kg(2,2)=ttt*sss*sss
	    kg(2,3)=ttt*ccc*sss*(-1)
	    kg(2,4)=ttt*sss*sss*(-1)
	    kg(3,1)=ttt*ccc*ccc*(-1)
	    kg(3,2)=ttt*ccc*sss*(-1)
	    kg(3,3)=ttt*ccc*ccc
	    kg(3,4)=ttt*ccc*sss
	    kg(4,1)=ttt*ccc*sss*(-1)
	    kg(4,2)=ttt*sss*sss*(-1)
	    kg(4,3)=ttt*ccc*sss
	    kg(4,4)=ttt*sss*sss
		
		!delta_el(1:4)=disp(2*element_backup(i,11)-1:2*element_backup(i,12))                                                                                                                                                                                                                                                                                                                                                                                                                                            
		delta_el(1)=disp(2*element_backup(i,11)-1)
		delta_el(2)=disp(2*element_backup(i,11))
		delta_el(3)=disp(2*element_backup(i,12)-1)
		delta_el(4)=disp(2*element_backup(i,12))
		scu(1)=fem(2*(element(i,11))-1)
		scu(2)=fem(2*(element(i,11))-0)
		scu(3)=fem(2*(element(i,12))-1)
		scu(4)=fem(2*(element(i,12))-0)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
		f_el=matmul(kg,delta_el)!-scu  
		if((element(i,4)-element(i,2))*f_el(1)>0)    truss="C"
		if((element(i,4)-element(i,2))*f_el(1)<0)    truss="T"
		if(f_el(1)==0.and.f_el(2)==0)				 truss="N"
		if((element(i,5)-element(i,3))*f_el(2)>0)	 truss="C"
		if((element(i,5)-element(i,3))*f_el(2)<0)	 truss="T"                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
	    write (1,*) i                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
	    write (3,*) i  
	    !QQQ= -1*(element(i,8)*element(i,10)/element(i,6))*(disp(2*element(i,11)-1)*cos(element(i,7))-disp(2*element(i,11))*sin(element(i,7))+disp(2*element(i,12)-1)*cos(element(i,7))+disp(2*element(i,12))*sin(element(i,7)))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
    !	write (*,217) QQQ , truss                                                                                                                                                                                                                                                                                                                                                                                                                                           
		write (1,217)  sqrt(f_el(1)**2+f_el(2)**2)  ,truss  
		write (3,217)  sqrt(f_el(1)**2+f_el(2)**2)  ,truss       
		QQQ=0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
	end do                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
	write (1,*)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
	write (3,*)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
	write (1,100) "Press return to proceed "                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
	result = getcharqq ( )                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
    call clearscreen($gclearscreen)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
	!nodal displacements                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
	write (1,102) "Output - Nodal displacements", "----------------------------"                                                                                                                                                                                                                                                                                                                                                                                                                                      
	write (1,103) "Nodal displacements in global co-ordinate system are as follows: "                                                                                                                                                                                                                                                                                                                                                                                                                                  
	write (3,103) "Nodal displacements in global co-ordinate system are as follows: "                                                                                                                                                                                                                                                                                                                                                                                                                                  
	write (1,*)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
	write (3,*)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
	write (1,106) "node num.", "x disp. (mm)", "y disp. (mm)"                                                                                                                                                                                                                                                                                                                                                                                                                                    
	write (3,106) "node num.", "x disp. (mm)", "y disp. (mm)"                                                                                                                                                                                                                                                                                                                                                                                                                                    
	write (1,101) "---------------------------------------------------"                                                                                                                                                                                                                                                                                                                                                                                                                        
	write (3,101) "---------------------------------------------------"                                                                                                                                                                                                                                                                                                                                                                                                                        
	do i=1,num_node                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
		write (1,115) i, disp(2*i-1), disp(2*i-0)                                                                                                                                                                                                                                                                                                                                                                                                                                                         
		write (3,115) i, disp(2*i-1), disp(2*i-0)                                                                                                                                                                                                                                                                                                                                                                                                                                                          
	end do                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
	write (3,101) "---------------------------------------------------------------------------"                                                                                                                                                                                                                                                                                                                                                                                                                        
	close(3)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
	WRITE (1,103) "AN APROXIMATE DISPLACED FRAME IS DRAWN IN THE WINDOOW TO THE RIGHT."
	    WRITE (1,101) "THE DEFLECTIONS IN THE DRAWING ARE MULTIPLIED BY 10."

	    !NEW NODE CO-ORDINATES
	    DO I=1,NUM_NODE
		    NODE(I,1)=NODE(I,1)+0.1*DISP(3*I-2)
		    NODE(I,2)=NODE(I,2)+0.1*DISP(3*I-1)
	    END DO

	    !LINEAR DRAWING OF THE DISPLACED FRAME
	    DO II=1,NUM_ELEM
		    J=FOCUSQQ(2)												!SELECTS WINDOW NUMBER 2 (GRAPHICS)
		    I=SETCOLOR (2442)											!SETS GRAPH COLOR TO GREY
		    L=LINE(NODE(ELEMENT(II,11),1),NODE(ELEMENT(II,11),2),NODE(ELEMENT(II,12),1),NODE(ELEMENT(II,12),2))
		    J=FOCUSQQ(1)												!BACK TO SCREEN NUMBER 1 (DATA)
    	END DO

	    WRITE (1,*)
	    WRITE (1,100) "PRESS ANY KEY TO PROCEED "
	    VERIFY= GETCHARQQ ( ) 
298	    call clearscreen($gclearscreen)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
CASE DEFAULT
    call clearscreen($gclearscreen)
    write(1,*) "WRONG ENTRY. PLEASE TRY AGAIN."
    goto    500
END SELECT

!********************************************************************************************************************************************
!FORMAT SPECIFICATIONS-----------------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************

100 FORMAT (X,A,X,\)										!QUESTION
101 FORMAT (X,A,X,I3,A)										!TEXT
102 FORMAT (/,X,A,/,X,A,/,X,A)
103 FORMAT (/,X,A,X,I3,A)									!RETURN+TEXT
104 FORMAT (/,X,A,X,\)										!RETURN+QUESTION
105 FORMAT (/,X,A,X,I3,A)
106 FORMAT (/,X,A,T20,A,T40,A,T60,A)						!SECTION PROPERTIES TABLE
107 FORMAT (X,F3.0,T20,ES15.4,T40,ES15.4,T60,ES15.5)		!SECTION PROPERTIES TABLE
108 FORMAT (/,X,5(A,A1))									!RETURN+USE OFASCII CODE IN THE TEXT
109 FORMAT (20(/,3X,A))										!LIST OF PEOPLE'S NAME
110 FORMAT (/,X,A1,20(A,A1))								!RETURN+ASCII
111 FORMAT (32X,A100)
112 FORMAT (58X,A30)
113 FORMAT (13(/))
114 FORMAT (X,5(A,A1))										!USE OF ASCII CODE IN THE TEXT
115 FORMAT (X,I3,T15,ES15.6,T35,ES15.6,T60,ES15.6)			!SECTION PROPERTIES TABLE
116 FORMAT (70X,A26)
117 FORMAT (X,T20,ES15.6,T40,ES15.6,T60,ES15.6)	            !NEWWWWWWWWWWWWWW
118 format (f7.3)              
217 format (x,t20,es15.6,t42,a1)	                       !newwwwwwwwwwwwww   
!********************************************************************************************************************************************
!GETCH---------------------------------------------------------------------------------------------------------------------------------------
!********************************************************************************************************************************************

	!THAT'S ALL FOLKS!
	B.TYPE=QWIN$MAX									!MAXIMIZE
	I=SETWSIZEQQ(1,B)								!SETS SCREEN SIZE
	WRITE (1,113)
	WRITE (1,111), "  ________  _____  _______ _____    ___    __    __       __________  __    __ _______ __"
	WRITE (1,111), " /_  __/ / / /   |/_  __( ) ___/   /   |  / /   / /      / ____/ __ \/ /   / //_/ ___// /"
	WRITE (1,111), "  / / / /_/ / /| | / /  |/\__ \   / /| | / /   / /      / /_  / / / / /   / ,<  \__ \/ / "
	WRITE (1,111), " / / / __  / ___ |/ /    ___/ /  / ___ |/ /___/ /___   / __/ / /_/ / /___/ /| |___/ /_/  "
	WRITE (1,111), "/_/ /_/ /_/_/  |_/_/    /____/  /_/  |_/_____/_____/  /_/    \____/_____/_/ |_/____(_)   "
	WRITE (1,111), "                                                                                         "
	WRITE (1,113)
	WRITE (1,116), "PRESS RETURN TO TERMINATE"
	VERIFY= GETCHARQQ ( ) 
	CALL SETMESSAGEQQ(CHAR(10)//"Exit Window?"//CHAR(10)//CHAR(10),QWIN$MSG_EXITQ )

	END PROGRAM FRAME_ANALYSIS



