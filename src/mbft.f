      Subroutine Mbft(iblk)
      implicit real*8 (a-h,o-z)
      character*12 fname,filename(20)
      character*12 fname1,fname2
      include 'partdata.h'
      logical filflgdisp(20)
      include 'memalloc.h'
      common /pool/ rpool(maxrpools),ipool(maxrpools)
      common /aa/ aa(maxaa)
      common /ia/ ia(maxia)
      dimension nodec(500)
c
      nsource=0
c
c      print*, 'bft',iblk

      ISTATUS = 1
      call openf(21,1,ISTATUS)

      call initrwf(21,iui,iur)
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
      
      ISTATUS = 11
      call openf(23,11,ISTATUS)
c     READ(21) TMAX,DT,TIME,IT
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
      
      t = time+dt
      time = time+dt
      it = it+1
      
      if (iblk.eq.0) then
      if (time.gt.tmax) then
      	call endjob(ierr)
      end if
      end if

      
      if (iblk.eq.0) then
      	print*, '******************'
      	print*, 'TMAX,SIMULATION TIME,DT,IT=',tmax,time,dt,it
      	call system_clock(int2)
      	print*, 'Time: ',int2, '(ms)'
      	int0 = int2

      end if
      
      ISTATUS = 11
      call openf(23,11,ISTATUS)
c     READ(21) TMAX,DT,TIME,IT
      call initrwf(23,iui,iur)
      iur = iur+1
      rpool(iur)=tmax
      iur = iur+1
      rpool(iur)=dt
      iur = iur+1
      rpool(iur)=time
      iui = iui+1
      ipool(iui)=it
      call endrwf(23,iui,iur)
c      print*, 'iblk',iblk,'time,tmax,it',time,tmax,it
      
      
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
      
c
C...... OPEN COOR0 FILE      
      ISTATUS = 2
      call openf(31,2,ISTATUS)
c     READ(25) NSKIP,KCOOR
      call initrwf(31,iui,iur)
      iui = iui+1
      nskip=ipool(iui)
      iui = iui+1
      kcoor=ipool(iui)
      call endrwf(31,iui,iur)
      
c      print*, 'bft',iblk,'ok'      
      
      
      
      kna1=kdgof*knode*1

      knb1=knode*1

      kna0=1
      kna1=kna1+kna0

      knb0=1
      knb1=knb1+knb0
      
      call bft(knode,kdgof,time,dt,nsource,numblk,
     *iblk,it,
     *aa(kna0),ia(knb0),
     *filename)

      return
      end
      
      
      subroutine bft(knode,kdgof,time,dt,nsource,numblk,
     *iblk,it,
     *bf,nodeall,
     *filename)
      implicit real*8 (a-h,o-z)
      character*12 filename(20)
      include 'memalloc.h'
      common /pool/ rpool(maxrpools),ipool(maxrpools)
        DIMENSION  bf(kdgof,knode),nodeall(knode)
        dimension nodec(10000),awave(10000)
      
c..... open nodeall file to get global node number
      ISTATUS = 8
      call openf(34,8,ISTATUS)
      call initrwf(34,iui,iur)
      iui = iui+1
      knode_g = ipool(iui)
      call endrwf(34,iui,iur)
      if (knode_g.ne.knode) then
      	print*, iblk,'BFT global knode cant match knode!!!'
      end if
      call initrwf(34,iui,iur)
      do i = 1,knode
      	iui = iui+1
      	nodeall(i) = ipool(iui)
      end do
      call endrwf(34,iui,iur)
      
c      if (iblk.eq.0) then
c      write(*,*) '**************'
c      write(*,*) (ipool(i),i=iui+1,iui+knode)
c      end if
      
c      if (iblk.eq.0) then
c      	write(*,*) 'bft 0'
c      	write(*,*) (nodeall(i),i=1,knode)
c      end if
      
            
      if (iblk.eq.nsource) then
      	open(31,file='constrain',status='unknown',form='formatted')
        read(31,*) nconstrain
	      do i = 1,nconstrain
	        read(31,*) nodec(i)
c        print *,nodec(i)
	      end do
        close(31)
        
        open(31,file='ricker.dat',status='unknown',form='formatted')
	        read(31,*)fmax, imodel
	        nwave = 0.

301	      read(31, *, end =302) tmp, tz
c	        print*, tmp 
c	        print*, tz 
	        nwave = nwave + 1
	        awave(nwave) = tz
	        goto 301
302       close(31)
      
        do ib = 1,numblk-1
        	call sendint(ib,nsource,nconstrain)
        	call sendai(ib,nsource,nodec(1),nconstrain)
        	call sendint(ib,nsource,nwave)
        	call sendint(ib,nsource,imodel)
        	call sendar(ib,nsource,awave(1),nwave)
        	call sendr(ib,nsource,fmax)
        end do
        
      else
        call recvint(iblk,nsource,nconstrain)
        call recvai(iblk,nsource,nodec(1),nconstrain)
        call recvint(iblk,nsource,nwave)
        call recvint(iblk,nsource,imodel)
        call recvar(iblk,nsource,awave(1),nwave)
        call recvr(iblk,nsource,fmax)
      
      end if
      
      wavemax = 0.d0
      do i = 1,nwave
      if(dabs(awave(i)).gt.wavemax) wavemax = dabs(awave(i))
      end do
      if(wavemax.lt.1.0e-15) wavemax = 1.0e-15
      do i = 1,nwave
      awave(i) = awave(i)/wavemax
      end do
      
      do i = 1,knode
      do j = 1,kdgof
      bf(j,i) = 0.d0
      end do
      end do
      
      if(imodel.eq.1) then
      omega= 100
      c1=1.5
      c2=3.0
      beta=2500
      pi=3.1415926536
      if(time.le.c2*pi/omega) then
      ft=fmax*sin(omega*time)*dexp(-1.d0*beta*(time-c1*pi/omega)**2.0)
      else
      ft=0.d0
      end if
      end if
c      

c      print*, iblk,(awave(i),i=1,3)
c      it = 2
      if(imodel.eq.2) then
      if(it.lt.nwave) then
      ft = awave(it)*fmax 
c      print*, 'ft',ft,fmax
      else
      ft = 0.d0
      end if
      end if
      
      
c     
      cita = 75.d0/180.d0*3.141592654                
	    fx = 0.d0
	    fy =  ft*sin(cita)
	    fz = -ft*cos(cita)
c        print *,n,cita,fx,fy,fz
      do i = 1,nconstrain
      icon = nodec(i)
      do j = 1,knode
      	inod = nodeall(j)
      	if (icon.eq.inod) then
c      		print*, 'iblk',iblk,imodel
     	    bf(1,j) = fx
    	    bf(2,j) = fy
    	    bf(3,j) = fz
c    	    print*, time,ft,fx,fy,fz
    	  end if
      end do
      end do
      
c.....write v file to common pool
      ISTATUS = 12
      call openf(35,12,ISTATUS)
      call initrwf(35,iui,iur)
      do j = 1,kdgof
      	do i = 1,knode
      		iur = iur+1
      		rpool(iur) = bf(j,i)
      	end do
      end do
      call endrwf(35,iui,iur)
      
c      if (iblk.eq.0) then
c      	write(*,*) ((bf(j,i),i=1,knode),j=1,kdgof)
c      end if
      
      return
      end 
      
      
      
      
      
      
      