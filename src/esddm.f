      subroutine Mesddm(iblk)
      implicit real*8 (a-h,o-z)
      character*12 fname,filename(20)
      character*12 fname1,fname2
      include 'partdata.h'
      logical filflgdisp(20)
      include 'memalloc.h'
      common /pool/ rpool(maxrpools),ipool(maxrpools)
      common /aa/ aa(maxaa)
      common /ia/ ia(maxia)
6       format(2x,10i5)
7       format(2x,6e12.5)


      master = 0
      nsource = master
      ierr = 0
      
      ISTATUS = 1
      call openf(1,1,ISTATUS)
c       read(21) numblk,numtyp,maxlm,nmdof,numtypl,kdgofl,keymt
      call initrwf(1,iui,iur)
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
      call endrwf(1,iui,iur)
c       read(21) lgio,t0,tmax,dt
      
      ISTATUS = 11
      call openf(23,11,ISTATUS)
c       read(21) tmax,dt,time,it
      call initrwf(23,iui,iur)
      iur = iur+1
      tmax=rpool(iur)
      iur = iur+1
      dt=rpool(iur)
      iur = iur+1
      time=rpool(iur)
      iui = iui+1
      it=ipool(iui)
      call endrwf(23,iui,iur)
     
      
      kna1=kdgof*knode
      kna2=kdgof*knode
      kna3=ncoor*knode
      kna4=nummat*mmat
      kna5=kdgof*knode
      kna6=kdgof*knode
      kna7=kdgof*knode

      knb1=kdgof*knode
      knb2=num*nne
      knb3=num*nne
      
      kna0=1
      kna1=kna1+kna0
      kna2=kna2+kna1
      kna3=kna3+kna2
      kna4=kna4+kna3
      kna5=kna5+kna4
      kna6=kna6+kna5
      kna7=kna7+kna6
	
      knb0=1
      knb1=knb1+knb0
      knb2=knb2+knb1
      knb3=knb3+knb2
      
c      print*, numblk,numtyp,numnode,knode,kdgof,num,nne,nummat,mmat,
c     * ncoor,time,dt,iblk
      
      call elwsa(numblk,numtyp,knode,kdgof,num,nne,nummat,mmat,
     * ncoor,time,dt,iblk,
     *aa(kna0),aa(kna1),aa(kna2),aa(kna3),aa(kna4),aa(kna5),aa(kna6),
     *ia(knb0),ia(knb1),ia(knb2),
     *filename)
      
      return
      end
      
      subroutine elwsa(numblk,numtyp,knode,kdgof,num,nne,nummat,mmat,
     * ncoor,time,dt,iblk,
     *u,f,coor,emate,eu,ev,emass,
     *nodvar,node,nodea,
     *filename)
      implicit real*8 (a-h,o-z)
      character*12 filename(20)
        include 'memalloc.h'
      common /pool/ rpool(maxrpools),ipool(maxrpools)
        DIMENSION NODVAR(KDGOF,KNODE),U(KDGOF,KNODE),COOR(nCOOR,KNODE),
     *eu(kdgof,knode),ev(kdgof,knode),Emass(kdgof*knode),nodea(num*nne),
     & F(kdgof*knode),EMATE(nummat*mmat),SML(10000000),NODE(num*nne)
 
6     FORMAT (1X,26I3)
7     FORMAT (1X,6E12.3)
1001  FORMAT(1X,9I7)
      
      ISTATUS = 2
      call openf(31,2,ISTATUS)
      call initrwf(31,iui,iur)
      iui = iui+1
      knode_c = ipool(iui)
      iui = iui+1
      ncoor_c = ipool(iui)
      call endrwf(31,iui,iur)
      if (knode_c .ne. knode) then
      	print*, iblk,'Coor number cant match!!!'
      end if
      if (ncoor_c .ne. ncoor) then
      	print*, iblk,'Ncoor number cant match!!!'
      end if
      call initrwf(31,iui,iur)
      do j = 1,knode
      	do i = 1,ncoor
      		iur = iur+1
      		coor(i,j) = rpool(iur)
      	end do
      end do

        
      
c
      ISTATUS = 3
      call openf(22,3,ISTATUS)
c     write (22) knode,kdgof
      call initrwf(22,iui,iur)
      iui = iui+1
      knode = ipool(iui)
      iui = iui+1
      kdgof = ipool(iui)
      call endrwf(22,iui,iur)
c     write (22) ((nodvar(i,j),i=1,kdgof),j=1,knode)
      call initrwf(22,iui,iur)
      do j=1,knode
      do i=1,kdgof
      iui = iui+1
      nodvar(i,j)=ipool(iui)
      end do
      end do
      call endrwf(22,iui,iur)
      
            
c      if (iblk.eq.2) then
c      	write(*,*) ((nodvar(i,j),i=1,kdgof),j=1,knode)
c      end if
      
      ISTATUS = 7
      call openf(25,7,ISTATUS)
c       read(25) (nodea(i),i=1,numnod)
      call initrwf(25,iui,iur)
      do i=1,num*nne
      iui = iui+1
      nodea(i)=ipool(iui)
      enddo
      call endrwf(25,iui,iur)
      
      call initrwf(25,iui,iur)
      do i=1,nummat*mmat
      iur = iur+1
      emate(i)=rpool(iur)
      enddo
      call endrwf(25,iui,iur)
      
      ISTATUS = 4
      call openf(21,4,ISTATUS)
      call initrwf(21,iui,iur)
      iui = iui+1
      knode_u = ipool(iui)
      iui = iui+1
      kdgof_u = ipool(iui)
      call endrwf(21,iui,iur)
      call initrwf(21,iui,iur)
      do j = 1,kdgof
      	do i = 1,knode
      		iur = iur+1
      		eu(j,i) = rpool(iur)
      	end do
      end do
      call endrwf(21,iui,iur)
      
      ISTATUS = 5
      call openf(33,5,ISTATUS)
      call initrwf(33,iui,iur)
      do j = 1,kdgof
      	do i = 1,knode
      		iur = iur+1
      		ev(j,i) = rpool(iur)
      	end do
      end do
      call endrwf(33,iui,iur)
      

      ISTATUS = 12
      call openf(35,12,ISTATUS)
      call initrwf(35,iui,iur)
      do j = 1,kdgof
      	do i = 1,knode
      		iur = iur+1
      		u(j,i) = rpool(iur)
      	end do
      end do
      call endrwf(35,iui,iur)
      
      
      NEQ=0
      DO 20 J=1,knode
c      DO 18 I=1,NODDOF
      DO 18 I=1,kdgof
      IF (NODVAR(I,J).LT.1) GOTO 18
      NEQ = NEQ + 1
18    CONTINUE
20    CONTINUE

c      if (iblk.eq.2) then
c      	write(*,*) 'neq=',neq
c      end if


      DO 110 I=1,NEQ
      Emass(i) = 0.0
110   CONTINUE
      
      NUMEL=0
      nodadd = 0
      nmadd = 0
      nnode = nne
      DO 2000 ITYP=1,NUMTYP
c     get the element data form nodea
        do i = 1,num
        	do j = 1,nnode
        		node((i-1)*nnode+j)=nodea(nodadd+(i-1)*nnode+j)
        	end do
        end do
        nodadd=nodadd+num*nnode
        
        knum=num*nnode
        if (knum.gt.num*nne) then
        write(*,*) 'Error, knum gt the default assigned dimension',knum
        stop 0000
        endif
      
      nne = nne-1
      K=0
      DO 116 J=1,NNE
      DO 115 L=1,KDGOF
       K=K+1
115   CONTINUE
116   CONTINUE
c
      kk=k*k
      k0=1
      k1=k0+k*k
      k2=k1+k
      k3=k2+k
      k4=k3+k
      k5=k4+k*k
      k6=k5+k*k
      k7=k6+k
      k8=k8+k
      
      
      CALL ETSUB(iblk,KNODE,KDGOF,IT,K,KK,NNODE,NNE,
     * NUMEL,ITYP,NCOOR,NUM,TIME,DT,nummat,mmat,
     * NODVAR,COOR,NODE,EMATE,
     *sml(k0),sml(k1),sml(k2),sml(k3),sml(k4),sml(k5),sml(k6),
     *sml(k7),
     *eu,ev,Emass,
     *U)
     
c        if(iblk.eq.1) then
c        	write(*,*) (emass(ij),ij=1,neq)
c        end if
     
      numel = numel + num
2000  CONTINUE

      emmax=0.0
      emmin=1.0e18
      do I=1,NEQ
c      write(22,*) emass(i)
      if (emmax.lt.emass(I)) emmax=emass(I)
      if (emmin.gt.emass(I)) emmin=emass(I)
      enddo
      
      emmin = emmax*1.d-8
      NEQ = 0
      DO 2050 IJ=1,KNODE*KDGOF
      if (emass(IJ).lt.emmin) emass(IJ)=emmin
2050  F(IJ)=0.0D0
      DO 2200 I=1,KNODE
      DO 2100 J=1,KDGOF
      IJ=NODVAR(J,I)
      IF (IJ.LE.0) GOTO 2100
      IF (IJ.GT.NEQ) NEQ = IJ
      F(IJ) = F(IJ)+U(J,I)/EMASS(IJ)
2100  CONTINUE
2200  CONTINUE
      DO 2400 I=1,KNODE
      DO 2300 J=1,KDGOF
      IJ=NODVAR(J,I)
	    ev(J,I) = U(J,I)
      IF (IJ.LE.0) GOTO 2300
      U(J,I) = F(IJ)
	    ev(J,I) = F(IJ)
2300  CONTINUE
2400  CONTINUE

      do 500 inod=1,knode
      do 501 idof=1,kdgof
      ij=nodvar(idof,inod)
      if (ij.le.0) then
        eu(idof,inod) = u(idof,inod)
      else
	      eu(idof,inod)=eu(idof,inod)+ev(idof,inod)*dt
      endif
501   continue
500   continue


c..... write eu and ev to common pool
      ISTATUS = 4
      call openf(21,4,ISTATUS)
      call initrwf(21,iui,iur)
      iui = iui+1
      knode = ipool(iui)
      iui = iui+1
      kdgof = ipool(iui)
      call endrwf(21,iui,iur)
      call initrwf(21,iui,iur)
      do j = 1,kdgof
      	do i = 1,knode
      		iur = iur+1
      		rpool(iur) = eu(j,i)
      	end do
      end do
      call endrwf(21,iui,iur)
      
      ISTATUS = 5
      call openf(33,5,ISTATUS)
      call initrwf(33,iui,iur)
      do j = 1,kdgof
      	do i = 1,knode
      		iur = iur+1
      		rpool(iur) = ev(j,i) 
      	end do
      end do
      call endrwf(33,iui,iur)

c      if (iblk.eq.0) then
c      	write(*,*) ((eu(j,i),i=1,knode),j=1,kdgof)
c        write(*,*) ((ev(j,i),i=1,knode),j=1,kdgof)
c      end if

       return
       end
       
       
       subroutine ETSUB(iblk,KNODE,KDGOF,IT,K,KK,NNODE,NNE,
     * NUMEL,ITYP,NCOOR,NUM,TIME,DT,nummat,mmat,
     * NODVAR,COOR,NODE,EMATE,
     *es,em,ec,ef,Estifn,Estifv,Emassn,Emassv,
     *eu,ev,Emass,
     * U)
      implicit real*8 (a-h,o-z)
      DIMENSION NODVAR(KDGOF,KNODE),COOR(NCOOR,KNODE),NODE(num*nne),
     & U(KDGOF,KNODE),EMATE(nummat*mmat),
     *es(k,k),em(k),ec(k),ef(k),eu(kdgof,knode),
     *ev(kdgof,knode),Estifn(k,k),Estifv(kk),Emassn(k),Emassv(k),
     *Emass(kdgof*knode),
     & R(500),PRMT(500),COEF(500),LM(500)
      common /tmn/ esa(50,50,8),ema(50,8),eca(50,8),
     *  efa(50,8),leg(8)
c     read data from common

      
        do ii = 1,8
        leg(ii) = 0
        end do
        DO 1000 NE=1,NUM
ccc        if(ne.eq.1) print *,'time  dt======',time,dt
ccc        if(ne.eq.num) print *,'time  dt======',time,dt
c        if(ne.eq.50000) print *,' 5% finished'
c        if(ne.eq.150000) print *,'15% finished'
c        if(ne.eq.250000) print *,'25% finished'
c        if(ne.eq.350000) print *,'35% finished'
c        if(ne.eq.500000) print *,'45% finished'
c        if(ne.eq.600000) print *,'55% finished'
c        if(ne.eq.700000) print *,'65%  finished'
c        if(ne.eq.800000) print *,'75% finished'
c        if(ne.eq.900000) print *,'85% finished'
c        if(ne.eq.1000000) print *,'95% finished'
        NR=0
        DO 130 J=1,NNE
        JNOD = NODE((NE-1)*NNODE+J)
        IF (JNOD.LT.0) JNOD = -JNOD
        DO 120 I=1,NCOOR
        NR=NR+1
120     R(NR) = COOR(I,JNOD)
130     CONTINUE
        IMATE = NODE(NNODE*NE)
        
c
c
c
        if(leg(imate).eq.0) then
        
        DO 140 J=1,mmat
140     PRMT(J) = EMATE((IMATE-1)*mmat+J)
        PRMT(mmat+1)=TIME
        PRMT(mmat+2)=DT
        PRMT(mmat+3)=IMATE
        prmt(mmat+4)=NE
        prmt(mmat+5)=NUM
        
c        if (iblk.eq.2) then
        	
c        	write(*,*) (prmt(i),i=1,mmat+5)
c        end if
        
      goto (1,2), ityp
1     call nuc8g2(r,coef,prmt,es,em,ec,ef,ne)
      goto 3
2     call agq4(r,coef,prmt,es,em,ec,ef,ne)
      goto 3
3     continue
      do i = 1,k
      do j = 1,k
      esa(i,j,imate) = es(i,j)
      end do
      end do
      do i = 1,k
      ema(i,imate) = em(i)
      end do
      do i = 1,k
      eca(i,imate) = ec(i)
      end do
      do i = 1,k
      efa(i,imate) = ef(i)
      end do
      leg(imate) = 1
      end if
c
c
c
      do i = 1,k
      do j = 1,k
      es(i,j) = esa(i,j,imate)
      end do
      end do
      do i = 1,k
      em(i) = ema(i,imate)
      end do
      do i = 1,k
      ec(i) = eca(i,imate)
      end do
      do i = 1,k
      ef(i) = efa(i,imate)
      end do
      
c
      do 201 i=1,k
      do 201 j=1,k
      Estifn(i,j)=0.0
201   continue
      do 202 i=1,k
      Estifn(i,i)=Estifn(i,i)
      do 202 j=1,k
      Estifn(i,j)=Estifn(i,j)+es(i,j)
202   continue
      do 203 i=1,k
      Emassn(i)=0.0
203   continue
      do 204 i=1,k
      Emassn(i)=Emassn(i)+em(i)+ec(i)*dt
204   continue

      NNODE=NNE+1
	    L=0
	    M=0
	    I=0
	    DO 700 INOD=1,NNE
	    NODI=NODE((NE-1)*NNODE+INOD)
	    DO 600 IDGF=1,KDGOF
	    INV=NODVAR(IDGF,NODI)
	    IF (INV.EQ.0) GOTO 600
	    I=I+1
    	IF (INV.LT.0) GOTO 305
    	L=L+1
    	LM(L)=INV
	    U(IDGF,NODI)=U(IDGF,NODI)
     * +ef(i)*dt+em(i)*ev(idgf,nodi)
      Emassv(l)=Emassn(i)
305     J=0
	    DO 500 JNOD=1,NNE
	    NODJ=NODE((NE-1)*NNODE+JNOD)
    	DO 400 JDGF=1,KDGOF
    	JNV=NODVAR(JDGF,NODJ)
    	IF (JNV.EQ.0) GOTO 400
    	J=J+1
 
    	IF (JNV.LT.0) GOTO 400
    	IF (INV.LT.0) GOTO 310
     	M=M+1
      Estifv(m)=Estifn(i,j)
310     CONTINUE
 
      u(jdgf,nodj)=u(jdgf,nodj)
     * -es(j,i)*eu(idgf,nodi)*dt
 
C	IF (INV.LT.0)
C     *  U(JDGF,NODJ)=U(JDGF,NODJ)-ESTIFN(J,I)*U(IDGF,NODI)
400     CONTINUE
500     CONTINUE
600     CONTINUE
700     CONTINUE

	    LRD=M
	    NER=NUMEL+NE
	    DO 800 I=1,L
    	J=LM(I)
      Emass(j) = Emass(j) + Emassv(i)
800     CONTINUE
      

 
 
1000    CONTINUE

      return 
      end
      