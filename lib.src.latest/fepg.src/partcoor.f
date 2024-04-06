      subroutine partcoor(knode,mx,my,mz,x,y,z,
     *no,nor,r,kblock,indx,indy,indz,ind)
      implicit real*8 (a-h,o-z)
      DIMENSION X(KNODE),Y(KNODE),Z(KNODE),R(KNODE),NO(KNODE),
     & INDX(*),INDY(*),INDZ(*),IND(*),NOR(KNODE),KBLOCK(KNODE)

6       FORMAT (1X, 15I5)
7       FORMAT (1X,6F13.4)

cC.......OPEN No. of DIV file
c      OPEN (1,FILE=' ',FORM='FORMATTED',STATUS='OLD')
c      READ (1,*) MX,MY,MZ
c      CLOSE(1)
c      write (*,*) MX,MY,MZ

cC.......OPEN COOR file
c      OPEN (1,FILE=' ',FORM='UNFORMATTED',STATUS='OLD')
c      READ (1) KNODE,NCOOR,(X(J),Y(J),Z(J),J=1,KNODE)
c      CLOSE(1)
c      WRITE(*,*) 'X,Y,Z ='
c      WRITE(*,*) (X(J),Y(J),Z(J),J=1,KNODE)

      do i=1,knode
        R(i)=X(i)
      enddo
      do i=1,knode
        no(i)=i
      enddo
      CALL DIV(KNODE,R,no,MX,IND)
c        WRITE(*,'(1X,10I6)') (no(I),I=1,KNODE)
      INDX(1)=0
      DO I=1,MX
       INDX(I+1) = INDX(I)+IND(I)
      ENDDO
 
      IY = 1
      DO I=1,MX
       N0=INDX(I)+1
       N1=INDX(I+1)
       NY=N1-N0+1
       DO N=1,NY
        K=N0+N-1
        L=No(K)
        R(N) = Y(L)
       ENDDO
       DO N=1,NY
        NoR(N)=N
       ENDDO
       CALL DIV(NY,R,NoR,MY,IND(IY))
       DO N=1,NY
        K=NOR(N)+N0-1
        NOR(N) = NO(K)
       ENDDO
       DO N=1,NY
        NO(N+N0-1) = NOR(N)
       ENDDO
       IY = IY+MY
      ENDDO
      INDY(1)=0
      DO I=1,MX*MY
       INDY(I+1) = INDY(I)+IND(I)
      ENDDO
 
      IZ = 1
      DO I=1,MX*MY
       N0=INDY(I)+1
       N1=INDY(I+1)
       NZ=N1-N0+1
       DO N=1,NZ
        K=N0+N-1
        L=NO(K)
        R(N) = Z(L)
       ENDDO
       DO N=1,NZ
        NoR(N)=N
       ENDDO
       CALL DIV(NZ,R,NOR,MZ,IND(IZ))   
        DO N=1,NZ
        K=NOR(N)+N0-1
        NOR(N) = NO(K)
       ENDDO
       DO N=1,NZ
        NO(N+N0-1) = NOR(N)
       ENDDO
       IZ = IZ+MZ
      ENDDO

      INDZ(1)=0
c      OPEN(10,FILE='',FORM='FORMATTED',STATUS='UNKNOWN')
      DO I=1,MX*MY*MZ
       INDZ(I+1) = INDZ(I)+IND(I)
       N0=INDZ(I)+1
       N1=INDZ(I+1)
c       WRITE(10,'(1X,10I6)') (NO(J),J=N0,N1)
       DO J=N0,N1
        K=NO(J)
        KBLOCK(K)=I-1
       ENDDO
      ENDDO
C      OPEN(10,FILE='',FORM='FORMATTED',STATUS='UNKNOWN')
c      WRITE(10,*) ' ----------------------------------'
c      WRITE(*,'(1X,10I6)') (KBLOCK(N),N=1,KNODE)
c     CLOSE(10)
 
      RETURN
      END
 
      SUBROUTINE DIV(KNODE,X,NO,MX,INDEX)
      implicit real (a-h,o-z)
      DIMENSION X(KNODE),NO(KNODE),INDEX(*)
c       CALL ORDER(KNODE,X,NO)
      ITEMP=KNODE-1
      call dqsort(X,No,0,ITEMP)
      NX = KNODE/MX
      ITEMP=KNODE-NX*MX
      DO I=1,ITEMP
       INDEX(I) = NX+1
      ENDDO
      DO I=ITEMP+1,MX
       INDEX(I) = NX
      ENDDO
C      INDEX(MX) = KNODE - (MX-1)*NX
      RETURN
      END

      SUBROUTINE DIV_lm(KNODE,X,NO,MX,INDEX)
      implicit real*8 (a-h,o-z)
      DIMENSION X(KNODE),NO(KNODE),INDEX(*)
c       CALL ORDER(KNODE,X,NO)
      tol=1.0d-10
      ITEMP=KNODE-1
      call dqsort(X,No,0,ITEMP)
      xmax=dabs(x(knode)-x(1))
      NX = KNODE/MX
      index(1)=nx
      if(index(1).ge.knode) goto 200
100   continue
      if(dabs( x(index(1)+1) - x(index(1))) .gt.tol) goto 200
      index(1)=index(1)+1
      goto 100
200   continue
      knode1=knode
      do i=2,mx-1
         knode1=knode1-index(i-1)
         nx=knode1/(mx-i+1)
         index(i)=nx
300      continue
         if(dabs( x(index(i)+1) - x(index(i))).gt.tol) goto 400
         index(i)=index(i)+1
         goto 300
400      continue
      enddo
      if(mx.ge.2) index(mx)=knode1-index(mx-1)
c      ITEMP=KNODE-NX*MX
c      DO I=1,MX-ITEMP
c       INDEX(I) = NX
c       do j=1,
c      ENDDO
c      DO I=ITEMP+1,MX
c       INDEX(I) = NX
c      ENDDO
cc      INDEX(MX) = KNODE - (MX-1)*NX
      RETURN
      END
 
c      SUBROUTINE ORDER(NR,R,NO)
c      implicit real*8 (a-h,o-z)
c      DIMENSION R(NR),NO(NR)
c      DO 300 K=1,NR
c         NO(K)=K
c300      CONTINUE     
c      DO 200 I=1,NR
c      RS=R(I)+1.0
c      DO 100 J=I,NR
c      IF (R(J).GT.RS) GOTO 100
c      RS=R(J)
c      J0=J
c100   CONTINUE
c      R(J0)=R(I)
c      R(I)=RS
c      IK=NO(I)
c      NO(I)=NO(J0)
c      NO(J0)=IK
c200   CONTINUE
cC     WRITE(*,*) (R(I),I=1,NR)
cC       WRITE(*,*) '-----------------'
c      RETURN
c      END
