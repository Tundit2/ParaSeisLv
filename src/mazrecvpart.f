      subroutine Mazrecvpart(iblk)
      implicit real*8 (a-h,o-z)
      character*12 fname,filename(20)
      character*12 fname1,fname2
      include 'partdata.h'
      logical filflgdisp(20)
      include 'memalloc.h'
      common /pool/ rpool(maxrpools),ipool(maxrpools)
      common /aa/ aa(maxaa)
      common /ia/ ia(maxia)
C
      if(iblk.eq.0) return
c
      nsource=0
c
      idisp1 = 0
      idisp2 = 0
c
c     Receive LMDDM file from master process
c
c      read(1) numblk,numtyp,knodeall,knode,kdgof,numnod,nummat,
c     & ncoor,idisp1,idisp2,t0,tmax,dt
c
      ISTATUS = 0
      call openf(1,1,ISTATUS)
      call initrwf(1,iui,iur)
      iui = iui+1
      call recvint(iblk,nsource,ipool(iui))
      numblk=ipool(iui)
      iui = iui+1
      call recvint(iblk,nsource,ipool(iui))
      numtyp=ipool(iui)
      iui = iui+1
      call recvint(iblk,nsource,ipool(iui))
      numnode=ipool(iui)
      iui = iui+1
      call recvint(iblk,nsource,ipool(iui))
      knode=ipool(iui)
      iui = iui+1
      call recvint(iblk,nsource,ipool(iui))
      kdgof=ipool(iui)
      iui = iui+1
      call recvint(iblk,nsource,ipool(iui))
      num=ipool(iui)
      iui = iui+1
      call recvint(iblk,nsource,ipool(iui))
      nne=ipool(iui)
      iui = iui+1
      call recvint(iblk,nsource,ipool(iui))
      nummat=ipool(iui)
      iui = iui+1
      call recvint(iblk,nsource,ipool(iui))
      mmat=ipool(iui)
      iui = iui+1
      call recvint(iblk,nsource,ipool(iui))
      ncoor=ipool(iui)
      iui = iui+1
      call recvint(iblk,nsource,ipool(iui))
      idisp1=ipool(iui)
      iui = iui+1
      call recvint(iblk,nsource,ipool(iui))
      idisp2=ipool(iui)
c
      iur = iur+1
      call recvr(iblk,nsource,rpool(iur))
      t0=rpool(iur)
      iur = iur+1
      call recvr(iblk,nsource,rpool(iur))
      tmax=rpool(iur)
      iur = iur+1
      call recvr(iblk,nsource,rpool(iur))
      dt=rpool(iur)
      call endrwf(1,iui,iur)
      
      
      ISTATUS=0
      call openf(23,11,ISTATUS)
      call initrwf(23,iui,iur)
      iur = iur+1
      rpool(iur)=tmax
      iur = iur+1
      rpool(iur)=dt
      iur=iur+1
      rpool(iur)=time
      iui=iui+1
      ipool(iui)=0
      call endrwf(23,iui,iur)
      
      
c
c      if(iblk.eq.2) then
c      write(*,*) 'iblk =====',iblk
c      write(*,*) 'numblk,numtyp,knode,kdgof,num,nne,nummat,mmat'
c      write(*,*)  numblk,numtyp,knode,kdgof,num,nne,nummat,mmat
c      write(*,*) 'ncoor,t0,tmax,dt'
c      write(*,*)  ncoor,t0,tmax,dt
c      end if
c
      kvar=knode*kdgof
c
c      write(*,*) 'knode,kdgof,kvar ='
c      write(*,'(1x,4i7)') knode,kdgof,kvar
c
c
      kna1=kdgof*knode*1
      kna2=kdgof*knode*1
      kna3=kdgof*knode*1
      kna4=ncoor*knode*1
      kna5=kdgof*knode*1
      kna6=nummat*mmat*1
      kna7=kdgof*knode*1
      kna8=maxaa*1
c
      knb1=num*nne*1
      knb2=kdgof*knode*1
      knb3=knode*1
      knb4=knode*1
      knb5=maxia*1
c
      kna0=1
      kna1=kna1+kna0
      kna2=kna2+kna1
      kna3=kna3+kna2
      kna4=kna4+kna3
      kna5=kna5+kna4
      kna6=kna6+kna5
      kna7=kna7+kna6
      kna8=maxaa - kna7
      maxrtemp = kna8
c
      knb0=1
      knb1=knb1+knb0
      knb2=knb2+knb1
      knb3=knb3+knb2
      knb4=knb4+knb3
      knb5=maxia-knb4
      maxitemp=knb5

c      write(*,*) 'Maxitemp, maxrtemp ===',maxitemp, maxrtemp 

      call azrecvpart(knode,kdgof,ncoor,numtyp,numnode,
     *numblk,iblk,nsource,num,nne,nummat,mmat,maxrtemp,maxitemp,
     *idisp1,idisp2,t0,tmax,dt,
     *aa(kna0),aa(kna1),aa(kna2),aa(kna3),aa(kna4),
     *aa(kna5),aa(kna6),aa(kna7),
     *ia(knb0),ia(knb1),ia(knb2),ia(knb3),ia(knb4),
     *filename)
 
      return
      end
      
      
      subroutine azrecvpart(knode,kdgof,ncoor,numtyp,numnode,
     *numblk,iblk,nsource,num,nne,nummat,mmat,maxrtemp,maxitemp,
     *idisp1,idisp2,t0,tmax,dt,
     *u0,u1,u2,coor,bfu,
     *emate,bfd,rtemp,
     *node,nodvar,nodeall,nid,itemp,
     *filename)
      implicit real*8 (a-h,o-z)
      character*12 filename(20)
        include 'memalloc.h'
      common /pool/ rpool(maxrpoolm),ipool(maxrpoolm)
      dimension u0(kdgof,knode),u1(kdgof,knode),u2(kdgof,knode),
     & coor(ncoor,knode),bfu(kdgof,knode),bfd(kdgof,knode),
     & emate(nummat*mmat),rtemp(maxrtemp)
       dimension  nodvar(kdgof,knode),nodeall(knode),nid(knode),
     *  itemp(maxitemp)
       dimension node(num*nne)
c
       logical filflgdisp(20)
c
c     Receive all the partitioned data from master process ... ...
c      
C     Receive and write coor0 file
C
C      read(31) knode,ncoor
C      read(31) ((coor(i,j),i=1,ncoor),J=1,knode)
c
      call recvint(iblk,nsource,knode)
      call recvint(iblk,nsource,ncoor)
      call recvar(iblk,nsource,rtemp(1),knode*ncoor)
c
c
      n = 0
      do j=1,knode
      do i=1,ncoor
      n = n+1
      coor(i,j) = rtemp(n)
      end do
      end do
c
      ISTATUS = 0
      call openf(31,2,ISTATUS)
      call initrwf(31,iui,iur)
      iui = iui+1
      ipool(iui)=knode
      iui = iui+1
      ipool(iui)=ncoor
      call endrwf(31,iui,iur)
c       write(31) ((coor(i,j),i=1,ncoor),j=1,knode)
      call initrwf(31,iui,iur)
      do j=1,knode
      do i=1,ncoor
      iur = iur+1
      rpool(iur)=coor(i,j)
      end do
      end do
      call endrwf(31,iui,iur)
c               
c
c       if(iblk.eq.3) then
c        write(*,*) 'coor0 ===knode,ncoor=',knode,ncoor
c        write(*,*)
c        do i = 1, knode
c        write(*,1100) i,(coor(j,i),j=1,ncoor)
c        end do
c       end if
c
C
C     Receive the ID0 file
C
c     read(42) knode,kdgof
c     read(42) ((nodvar(i,j),i=1,kdgof),j=1,knode)
c
        call recvint(iblk,nsource,knode)
        call recvint(iblk,nsource,kdgof)
        call recvai(iblk,nsource,itemp(1),knode*kdgof)
c  
        n = 0
        do j=1,knode
        do i=1,kdgof
        n = n+1
        nodvar(i,j) = itemp(n)
        end do
        end do
c
      ISTATUS = 0
      call openf(22,3,ISTATUS)
c     write (22) knode,kdgof
      call initrwf(22,iui,iur)
      iui = iui+1
      ipool(iui) = knode
      iui = iui+1
      ipool(iui) = kdgof
      call endrwf(22,iui,iur)
c     write (22) ((nodvar(i,j),i=1,kdgof),j=1,knode)
      call initrwf(22,iui,iur)
      do j=1,knode
      do i=1,kdgof
      iui = iui+1
      ipool(iui) = nodvar(i,j)
      end do
      end do
      call endrwf(22,iui,iur)
c
c      if(iblk.eq.3) then
c        write(*,*) 'id0 ===knode,kdgof=',knode,kdgof
c        write(*,*)
c        do i = 1, knode
c        write(*,1000) i,(nodvar(j,i),j=1,kdgof)
c        end do
c      end if
c     
C
C     Receive the constrained boundary file (disp0) data 
C
c     read(43) knode,kdgof
c     read(43) ((bfu(i,j),i=1,kdgof),j=1,knode)
        call recvint(iblk,nsource,knode)
        call recvint(iblk,nsource,kdgof)
        call recvar(iblk,nsource,rtemp(1),knode*kdgof)
c  
        n = 0
        do j=1,knode
        do i=1,kdgof
        n = n+1
        bfu(i,j) = rtemp(n)
        end do
        end do
c
      ISTATUS = 0
      call openf(21,4,ISTATUS)
c     write(21)knode,kdgof
      call initrwf(21,iui,iur)
      iui = iui+1
      ipool(iui) = knode
      iui = iui+1
      ipool(iui) = kdgof
      call endrwf(21,iui,iur)
      call initrwf(21,iui,iur)
      do i=1,kdgof
      do j=1,knode
      iur = iur+1
      rpool(iur) = bfu(i,j)
      end do
      end do
      call endrwf(21,iui,iur)
c
c      if(iblk.eq.8) then
c        write(*,*) 'disp0 ===knode,kdgof=',knode,kdgof
c        write(*,*)
c        do i = 1, knode
c        write(*,1000) i,(bfu(j,i),j=1,kdgof)
c        end do
c      end if
C        
C
C    send out the disp1 file (Initial values of development equations ... ...)
C
c    read(44) knode,kdgof
c    read(44) ((u0(i,j),i=1,kdgof),j=1,knode)
        call recvint(iblk,nsource,knode)
        call recvint(iblk,nsource,kdgof)
        call recvar(iblk,nsource,rtemp(1),knode*kdgof)
c  
        n = 0
        do j=1,knode
        do i=1,kdgof
        n = n+1
        u0(i,j) = rtemp(n)
        end do
        end do
        
        ISTATUS=0
        call openf(33,5,ISTATUS)
        call initrwf(33,iui,iur)
        do j = 1,kdgof
      	do i = 1,knode
      		iur = iur+1
      		rpool(iur) = U0(j,i)
      	end do
        end do
        call endrwf(33,iui,iur)

C
C       receive ETYPE0 file
C
c       read(45) numtyp,numnod,nummat
c       read(45) (idet(i),numa(i),nnea(i),mmta(i),nmta(i),i=1,numtyp),
c     & (inode(i),i=1,knode)
        call recvint(iblk,nsource,num)
        call recvint(iblk,nsource,nne)
        call recvint(iblk,nsource,nummat)
        call recvint(iblk,nsource,mmat)
c        
c
      ISTATUS = 0
      call openf(30,9,ISTATUS)
      call initrwf(30,iui,iur)
      iui = iui+1
      ipool(iui) = num
      iui = iui+1
      ipool(iui) = nne
      iui = iui+1
      ipool(iui) = nummat
      iui = iui+1
      ipool(iui) = mmat
c
      call endrwf(30,iui,iur)   
c
c      if(iblk.eq.2) then
c      write(*,*) 'Element information =numnod,nummat=',numnod,nummat
c      write(*,*)
c      do i = 1, numtyp
c      write(*,*) i,idet(i),numa(i),nnea(i),mmta(i),nmta(i)
c      end do
c      end if
C               
c
C     Receive elem0 file

c
       call recvai(iblk,nsource,itemp(1),1*num*nne)
       call recvar(iblk,nsource,rtemp(1),1*nummat*mmat)
c     
       iui = 0
       do i=1,num*nne
       iui = iui+1
       node(i) = itemp(iui)
       enddo
c       
       iur = 0
       do i=1,nummat*mmat
       iur = iur+1
       emate(i) = rtemp(iur)
       enddo
c
c
c.... write elem0 file
c
      ISTATUS = 0
      call openf(25,7,ISTATUS)
c     write(25) (nodeall(i),i=1,numnod)
      call initrwf(25,iui,iur)
      do i=1,num*nne
      iui = iui+1
      ipool(iui) = node(i)
      enddo
      call endrwf(25,iui,iur)
      if(nummat.gt.0) then

      call initrwf(25,iui,iur)
      do i=1,nummat*mmat
      iur = iur+1
      rpool(iur) = emate(i)
      end do
      call endrwf(25,iui,iur)
      end if
      
c      if(iblk.eq.2) then
c      	write(*,*) (emate(i),i=1,mmat*nummat)
c      end if
      
     
c 
c......receive index of subdomain gross equation number ... ...
c
        call recvint(iblk,nsource,knode)
        call recvai(iblk,nsource,itemp(1),knode)
c      return
c  
        n = 0
        do j=1,knode
        n = n+1
        nodeall(n) = itemp(n)
        end do
c
      call recvai(iblk,nsource,itemp(1),knode)
c  
        n = 0
        do j=1,knode
        n = n+1
        nid(n) = itemp(n)
        end do


      ISTATUS = 0
      call openf(34,8,ISTATUS)
c     write (22) knode,kdgof
      call initrwf(34,iui,iur)
      iui = iui+1
      ipool(iui) = knode
      call endrwf(34,iui,iur)
      call initrwf(34,iui,iur)
      do j=1,knode
      iui = iui+1
      ipool(iui) = nodeall(j)
      end do
      call endrwf(34,iui,iur)
      call initrwf(34,iui,iur)
      do j=1,knode
      iui = iui+1
      ipool(iui) = nid(j)
      end do
      call endrwf(34,iui,iur)
      
c      if (iblk.eq.2) then
c      	write(*,*) (nodeall(i),i=1,knode)
c      	write(*,*) (nid(i),i=1,knode)
c      end if
      
      do j = 1,kdgof
      	do i = 1,knode
      		bfd(j,i) = 0.0
      	end do
      end do
      
      ISTATUS = 0
      call openf(35,12,ISTATUS)
c     write(21)knode,kdgof
      call initrwf(35,iui,iur)
      do j=1,kdgof
      do i=1,knode
      iur = iur+1
      rpool(iur) = bfd(j,i)
      end do
      end do
      call endrwf(35,iui,iur)
c

C
C    Receive DISP2 for INITIAL DU/DT
C
      if (idisp1.eq.1) then
c     read(48)knode,kdgof
c     read(48)((u1(i,j),i=1,kdgof),j=1,knode)
        call recvint(iblk,nsource,knode)
        call recvint(iblk,nsource,kdgof)
        call recvar(iblk,nsource,rtemp(1),knode*kdgof)
c  
        n = 0
        do j=1,knode
        do i=1,kdgof
        n = n+1
        u1(i,j) = rtemp(n)
        end do
        end do
c
      ISTATUS = 0
      call openf(21,9,ISTATUS)
c     write(21)knode,kdgof
      call initrwf(21,iui,iur)
      iui = iui+1
      ipool(iui) = knode
      iui = iui+1
      ipool(iui) = kdgof
      call endrwf(21,iui,iur)
      call initrwf(21,iui,iur)
      do j=1,knode
      do i=1,kdgof
      iur = iur+1
      rpool(iur) = u1(i,j)
      end do
      end do
      call endrwf(21,iui,iur)
c
c     if(iblk.eq.8) then
C        write(*,*) 'disp2 ===knode,kdgof=',knode,kdgof
C        write(*,*)
C        do i = 1, knode
C        write(*,1000) i,(u1(j,i),j=1,kdgof)
C        end do
c     end if
C        
        endif
C
C    Receive DISP3 FOR INITIAL (DU/DT)2
C
      if (idisp2.eq.1) then
c     read(49)knode,kdgof
c     read(49)((u2(i,j),i=1,kdgof),j=1,knode)
        call recvint(iblk,nsource,knode)
        call recvint(iblk,nsource,kdgof)
        call recvar(iblk,nsource,rtemp(1),knode*kdgof)
c  
        n = 0
        do j=1,knode
        do i=1,kdgof
        n = n+1
        u2(i,j) = rtemp(n)
        end do
        end do
c
      ISTATUS = 0
      call openf(21,10,ISTATUS)
c     write(21)knode,kdgof
      call initrwf(21,iui,iur)
      iui = iui+1
      ipool(iui) = knode
      iui = iui+1
      ipool(iui) = kdgof
      call endrwf(21,iui,iur)
      call initrwf(21,iui,iur)
      do j=1,knode
      do i=1,kdgof
      iur = iur+1
      rpool(iur) = u2(i,j)
      end do
      end do
      call endrwf(21,iui,iur)
c
c     if(iblk.eq.8) then
C        write(*,*) 'disp3 ===knode,kdgof=',knode,kdgof
C        write(*,*)
C        do i = 1, knode
C        write(*,1000) i,(u2(j,i),j=1,kdgof)
C        end do
c     end if
C        
        endif
C
C
c
1000    format(10i10)
1100    format(i6,8e16.5)
1200    format(6i10)
      return
      end
