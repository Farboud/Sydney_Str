
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!SUBROUTINES---------------------------------------------------------------------------------------------------------------------------------
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS

SUBROUTINE STIFFNESS(E,I,A,L,KL)			!FORMS THE LOCAL STIFFNESS MATRIX OF A FRAME ELEMENT
	REAL*8 E,I,A,L
	REAL*8, DIMENSION(6,6)::KL
	DO QQ=1,6
		DO RR=1,6
			KL(QQ,RR)=0
		END DO
	END DO
	KL(1,1)=E*A/l
	KL(4,1)=(-1)*E*A/L
	KL(2,2)=12*E*I/(L**3)
	KL(3,2)=(-6)*E*I/(L**2)
	KL(5,2)=(-12)*E*I/(L**3)
	KL(6,2)=(-6)*E*I/(L**2)
	KL(3,3)=4*E*I/L
	KL(5,3)=6*E*I/(L**2)
	KL(6,3)=2*E*I/L
	KL(4,4)=E*A/L
	KL(5,5)=12*E*I/(L**3)
	KL(6,5)=6*E*I/(L**2)
	KL(6,6)=4*E*I/L
	KL(1,4)=(-1)*E*A/L
	KL(2,3)=(-6)*E*I/(L**2)
	KL(2,5)=(-12)*E*I/(L**3)
	KL(2,6)=(-6)*E*I/(L**2)
	KL(3,5)=6*E*I/(L**2)
	KL(3,6)=2*E*I/L
	KL(5,6)=6*E*I/(L**2)
END SUBROUTINE STIFFNESS

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE TURN(C,S,T)					!Forms the Transposition Matrix of a frame element
	REAL*8 C
	REAL*8 S
	REAL*8 , DIMENSION (6,6)::T
	DO QQ=1,6
		DO RR=1,6
			T(QQ,RR)=0
		END DO
	END DO
	T(1,1)=C
	T(2,1)=(-1)*S
	T(1,2)=S
	T(2,2)=C
	T(3,3)=1
	T(4,4)=C
	T(5,4)=(-1)*S
	T(4,5)=S
	T(5,5)=C
	T(6,6)=1
END SUBROUTINE TURN

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS

SUBROUTINE INVERSE (MAT,INVMAT,N)       !INVERSE OF MATRIX BY GAUSS-JORDAN METHOD 
	IMPLICIT NONE
	INTEGER::N
	REAL*8::MAT(N,N), INVMAT(N,N), TEMPMAT(N,N)
	!LOCAL VARIABLES DECLERATIONS
	REAL*8::B(N,N), C, D, TEMP(N), DET
	INTEGER::I,J,K,M,IMAX(1),IPVT(N)

	TEMPMAT=MAT
	B=MAT
	IPVT=(/(I,I=1,N)/)
	DO K=1,N
		IMAX=MAXLOC(ABS(B(K:N,K)))
		M=K-1+IMAX(1)
		IF (M/=K) THEN
			IPVT((/M,K/))=IPVT((/K,M/))
			B((/M,K/),:)=B((/K,M/),:)
		END IF
		D=1/B(K,K)
		TEMP=B(:,K)
		DO J=1,N
			C=B(K,J)*D
			B(:,J)=B(:,J)-TEMP*C
			B(K,J)=C
		END DO
		B(:,K)=TEMP*(-D)
		B(K,K)=D
	END DO
	MAT(:,IPVT)=B
	INVMAT=MAT
	MAT=TEMPMAT
END SUBROUTINE INVERSE

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS

!----------------------------------------------------------------------------
Subroutine Determinant(matrix, Det, n)		!Matrix Determinant Calculator
!----------------------------------------------------------------------------
    Implicit none
	Integer, Intent(in) :: n
    Real(8), Dimension(n,n) :: matrix
    Real :: m, temp
	Real(8)::Det
    Integer :: i, j, k, l
    Logical :: DetExists = .TRUE.
    l = 1
    !Convert to upper triangular form
    Do k = 1, n-1
        If (matrix(k,k) == 0) THEN
            DetExists = .FALSE.
            Do i = k+1, n
                If (matrix(i,k) /= 0) THEN
                    Do j = 1, n
                        temp = matrix(i,j)
                        matrix(i,j)= matrix(k,j)
                        matrix(k,j) = temp
                    End Do
                    DetExists = .TRUE.
                    l=-l
                    Exit
                End if
            End Do
            If (DetExists .EQV. .FALSE.) Then
                det = 0
                Return
            End If
        End If
        Do j = k+1, n
            m = matrix(j,k)/matrix(k,k)
            Do i = k+1, n
                matrix(j,i) = matrix(j,i) - m*matrix(k,i)
            End Do
        End Do
    End Do
   
    !Calculation determinant by finding product of diagonal elements
    det = l
    Do i = 1, n
        det = det * matrix(i,i)
    End Do
   
End subroutine Determinant

!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
SUBROUTINE KGELOBAL (E,I,A,L,C,S,KG)
REAL*8 E,I,A,L,C,S
	REAL*8, DIMENSION(6,6)::KG
	DO QQ=1,6
		DO RR=1,6
			KG(QQ,RR)=0
		END DO
	END DO
KG(1,1)=(E*A*(C**2)/L)+(12*E*I*(S**2)/(L**3))
KG(1,2)=(E*A*C*S/L)-(12*E*I*C*S/(L**3))
KG(1,3)=(-6*E*I*S/(L**2))
KG(1,4)=((-1)*E*A*(C**2)/L)-(12*E*I*(S**2)/(L**3))
KG(1,5)=((-1)*E*A*C*S/L)+(12*E*I*C*S/(L**3))
KG(1,6)=(-6*E*I*S/(L**2))
KG(2,2)=(E*A*(S**2)/L)+(12*E*I*(C**2)/(L**3))
KG(2,3)=(6*E*I*C/(L**2))
KG(2,4)=((-1)*E*A*C*S/L)+(12*E*I*C*S/(L**3))
KG(2,5)=((-1)*E*A*(S**2)/L)-(12*E*I*(C**2)/(L**3))
KG(2,6)=(6*E*I*C/(L**2))
KG(3,3)=(4*E*I/L)
KG(3,4)=(6*E*I*S/(L**2))
KG(3,5)=(-6*E*I*C/(L**2))
KG(3,6)=(2*E*I/L)
KG(4,4)=(E*A*(C**2)/L)+(12*E*I*(S**2)/(L**3))
KG(4,5)=(E*A*C*S/L)-(12*E*I*C*S/(L**3))
KG(4,6)=(6*E*I*S/(L**2))
KG(5,5)=(E*A*(S**2)/L)+(12*E*I*(C**2)/(L**3))
KG(5,6)=(-6*E*I*C/(L**2))
KG(6,6)=(4*E*I/L)
	DO m=2,6
		Do n=1,m-1
		KG(m,n)=KG(n,m)
		END DO
	END DO

END SUBROUTINE KGELOBAL
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS


