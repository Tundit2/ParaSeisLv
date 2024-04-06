      subroutine Mstart(iblk,itter)
      implicit real*8 (a-h,o-z)
      character*12 fname,filename(20)
      character*12 fname1,fname2
      include 'partdata.h'
      logical filflgdisp(20)
      include 'memalloc.h'
      common /pool/ rpool(maxrpools),ipool(maxrpools)
      common /aa/ aa(maxaa)
      common /ia/ ia(maxia)
c      
      nsource = 0
c      
c      print*, 'start',iblk,itter
      
      ISTATUS = 1
      call openf(21,1,ISTATUS)
      
      call initrwf(21,iui,iuf)
      iui = iui+1
      numblk=ipool(iui)
      iui = iui+1
      numtyp=ipool(iui)
      iui = iui+1
      numnode=ipool(iui)
      iui = iui+1
      knode=ipool(iui)
      iui = iui+1
      kdgof=ipool(iui)
      iui = iui+1
      num=ipool(iui)
      iui = iui+1
      nne=ipool(iui)
      iui = iui+1
      nummat=ipool(iui)
      iui = iui+1
      mmat=ipool(iui)
      iui = iui+1
      ncoor=ipool(iui)
      iui = iui+1
      idisp1=ipool(iui)
      iui = iui+1
      idisp2=ipool(iui)
      iur = iur+1
      t0=rpool(iur)
      iur = iur+1
      tmax=rpool(iur)
      iur = iur+1
      dt=rpool(iur)
      call endrwf(21,iui,iur)
c      
c      if(iblk.eq.2) then
c      write(*,*) 'iblk =====',iblk
c      write(*,*) 'numblk,numtyp,knode,kdgof,num,nne,nummat,mmat'
c      write(*,*)  numblk,numtyp,knode,kdgof,num,nne,nummat,mmat
c      write(*,*) 'ncoor,t0,tmax,dt'
c      write(*,*)  ncoor,t0,tmax,dt
c      end if
      
c      
      
C...... OPNE DISP0 FILE
      ISTATUS = 4
      call openf(21,4,ISTATUS)
c       READ(21) KNODE,KDGOF
      call initrwf(21,iui,iur)
      iui = iui+1
      knode=ipool(iui)
      iui = iui+1
      kdgof=ipool(iui)
      call endrwf(21,iui,iur)
      KVAR=KNODE*KDGOF
      
      kna1=kdgof*knode*1
      kna2=kdgof*knode*1
      kna3=kdgof*knode*1
c     
      knb1=kdgof*knode*1
      knb2=knode*1
c
      kna0=1
      kna1=kna1+kna0
      kna2=kna2+kna1
      kna3=kna3+kna2
c
      knb0=1
      knb1=knb1+knb0
      knb2=knb2+knb1

c      
      call start(knode,kdgof,iblk,nsource,
     *aa(kna0),aa(kna1),aa(kna2),
     *ia(knb0),ia(knb1),
     *filename)
c
      return
      end
      
      
      subroutine start(knode,kdgof,iblk,nsource,
     *bfu,eu,ev,
     *nodvar,inodvar,
     *filename)
      implicit real*8 (a-h,o-z)
      character*12 filename(20)
      include 'memalloc.h'
      common /pool/ rpool(maxrpools),ipool(maxrpools)
      dimension  bfu(kdgof,knode),eu(kdgof,knode),ev(kdgof,knode),
     *nodvar(kdgof,knode),inodvar(knode)
      
6     FORMAT (1X,10I10)
7     FORMAT (1X,6E12.5)
      
      
C...... READ ID0 FILE
      ISTATUS = 3
      call openf(22,3,ISTATUS)
c       READ (22) NSKP,NSKP
      call initrwf(22,iui,iur)
      iui = iui+1
      nskp=ipool(iui)
      iui = iui+1
      nskp=ipool(iui)
      call endrwf(22,iui,iur)
c       READ (22) ((NODVAR(I,J),I=1,KDGOF),J=1,KNODE)
      call initrwf(22,iui,iur)
      do J=1,KNODE
      do I=1,KDGOF
      iui = iui+1
      nodvar(I,J)=ipool(iui)
      enddo
      enddo
      call endrwf(22,iui,iur)
      
      DO 12 N=1,KNODE
      INODVAR(N)=N
12    CONTINUE
      
      
        NEQ=0
        DO 20 J=1,KNODE
          J1=INODVAR(J)
          DO 18 I=1,KDGOF
            IF (NODVAR(I,J1).LT.1) GOTO 18
            NEQ = NEQ + 1
            NODVAR(I,J1) = NEQ
18        CONTINUE
20      CONTINUE
        DO 30 J=1,KNODE
          J1=INODVAR(J)
          DO 28 I=1,KDGOF
            IF (NODVAR(I,J1).GE.-1) GOTO 28
            N = -NODVAR(I,J1)-1
27          CONTINUE
            IF (NODVAR(I,N).LT.-1) THEN
              N=-NODVAR(I,N)-1
              GOTO 27
            ENDIF
            NODVAR(I,J1) = NODVAR(I,N)
28        CONTINUE
30      CONTINUE
      
c
C.......WRITE NV FILE
c
      ISTATUS=3
      call openf(22,3,ISTATUS)
c       WRITE(23) NEQ
      call initrwf(22,iui,iur)
      iui = iui+1
      ipool(iui)=knode
      iui = iui+1
      ipool(iui)=kdgof
      call endrwf(22,iui,iur)
c       WRITE(23) ((NODVAR(I,J),I=1,KDGOF),J=1,KNODE)
      call initrwf(22,iui,iur)
      do J=1,KNODE
      do I=1,KDGOF
      iui = iui+1
      ipool(iui)=nodvar(I,J)
      enddo
      enddo
      call endrwf(22,iui,iur)
      
c      if (iblk.eq.2) then
c      	write(*,*) ((nodvar(i,j),i=1,kdgof),j=1,knode)
c      end if
      
c
c...... READ DISP0 FILE
c       READ(21) ((BFU(I,J),I=1,KDGOF),J=1,KNODE)
c
      ISTATUS = 4
      call openf(21,4,ISTATUS)
      call initrwf(21,iui,iur)
      iui = iui+1
      nskp=ipool(iui)
      iui = iui+1
      nskp=ipool(iui)
      call endrwf(21,iui,iur)
      call initrwf(21,iui,iur)
      do I=1,KDGOF
      do J=1,KNODE
      iur = iur+1
      bfu(I,J)=rpool(iur)
      enddo
      enddo
      call endrwf(21,iui,iur)
      
c      print*, 'start',iblk      
      
      do j = 1,kdgof
      	do i = 1,knode
      		eu(j,i) = 0.0
      		ev(j,i) = 0.0
      	end do
      end do
      
c.....write eu and ev file
      ISTATUS = 4
      call openf(21,4,ISTATUS)
      call initrwf(21,iui,iur)
      iui = iui+1
      ipool(iui)=knode
      iui = iui+1
      ipool(iui)=kdgof
      call endrwf(21,iui,iur)
      call initrwf(21,iui,iur)
      do I=1,KDGOF
      do J=1,KNODE
      iur = iur+1
      rpool(iur)=eu(I,J)
      enddo
      enddo
      call endrwf(21,iui,iur)
      
      ISTATUS = 5
      call openf(33,5,ISTATUS)
      call initrwf(33,iui,iur)
      do I=1,KDGOF
      do J=1,KNODE
      iur = iur+1
      rpool(iur)=ev(I,J)
      enddo
      enddo
      call endrwf(33,iui,iur)
      
      
      return 
      end
      
      
      