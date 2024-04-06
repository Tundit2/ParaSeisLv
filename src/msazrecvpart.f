      subroutine Mmsazrecvpart(iblk)
      implicit real*8 (a-h,o-z)
      character*12 fname,filename(20)
      character*12 fname1,fname2
      include 'partdata.h'
      logical filflgdisp(20)
      include 'memalloc.h'
      common /pool/ rpool(maxrpoolm),ipool(maxrpoolm)
      common /aa/ aa(maxaa)
      common /ia/ ia(maxia)
c
      if(iblk.ne.0) then
      write(*,*) 'Error, wrong calling subroutine ...'
      write(*,*) 'Should be run on master process, but'
      write(*,*) 'called as slave process of',iblk
      return
      end if
c
      nsource=0
c
      idisp1 = 0
      idisp2 = 0
      open(1,file='mlmddm',form='unformatted',status='unknown')
      read(1) numblk,numtyp,numnode,knode,kdgof,num,nne,
     & nummat,mmat,ncoor,idisp1,idisp2,t0,tmax,dt
      close(1)
c
      ISTATUS = 0
      call openf(1,1,ISTATUS)
      call initrwf(1,iui,iur)
      iui = iui+1
      ipool(iui) = numblk
      iui = iui+1
      ipool(iui) = numtyp
      iui = iui+1
      ipool(iui) = numnode
      iui = iui+1
      ipool(iui) = knode
      iui = iui+1
      ipool(iui) = kdgof
      iui = iui+1
      ipool(iui) = num
      iui = iui+1
      ipool(iui) = nne
      iui = iui+1
      ipool(iui) = nummat
      iui = iui+1
      ipool(iui) = mmat
      iui = iui+1
      ipool(iui) = ncoor
      iui = iui+1
      ipool(iui) = idisp1
      iui = iui+1
      ipool(iui) = idisp2
      iur = iur+1
      rpool(iur) = t0
      iur = iur+1
      rpool(iur) = tmax
      iur = iur+1
      rpool(iur) = dt
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
c      if(iblk.eq.0) then
c      write(*,*) 'iblk =====',iblk
c      write(*,*) 'numblk,numtyp,knodeall,knode,kdgof,numnod,nummat'
c      write(*,*)  numblk,numtyp,knodeall,knode,kdgof,numnod,nummat
c      write(*,*) 'ncoor,t0,tmax,dt'
c      write(*,*)  ncoor,t0,tmax,dt
c      end if
C
c
c     open data file from master processor
c
c
c     open data file from master processor
c
C...... OPEN LMDDM FILE
        OPEN (1,FILE='mlmddm',FORM='UNFORMATTED',STATUS='UNKNOWN')
C...... OPEN COOR0 FILE
        OPEN (30,FILE='mcoor0',FORM='UNFORMATTED',STATUS='UNKNOWN')
C...... OPEN ID0 FILE
        OPEN (31,FILE='mid0',FORM='UNFORMATTED',STATUS='UNKNOWN')
C...... OPNE DISP0 FILE (Boundary condition file)
        OPEN (32,FILE='mdisp0',FORM='UNFORMATTED',STATUS='UNKNOWN')
C...... OPNE DISP1 FILE (Initial value file for displacement)
        OPEN (33,FILE='mdisp1',FORM='UNFORMATTED',STATUS='UNKNOWN')
C...... OPEN ELEM0 FILE
        OPEN (35,FILE='melem0',FORM='UNFORMATTED',STATUS='UNKNOWN')
C...... OPEN subnodes0 file for contact boundary of each subdomain
        OPEN (36,file='nodeall0',form='unformatted',status='unknown')
C
C     open data file for other initial value problem
C
C...... OPNE DISP2 FILE (Initial value file for velocity)
      if (idisp1.eq.1) then
      open(37,file='mdisp2',form='unformatted',status='unknown')
      endif
C...... OPNE DISP3 FILE (Initial value file for acceleration)
      if (idisp2.eq.1) then
      open(38,file='mdisp3',form='unformatted',status='unknown')
      endif
C
      read (30) knode,ncoor
      read (31) knode,kdgof
      read (35) num,nne
      if(iblk.ne.0) then
      write(*,*) 'Unmatched subdomain data being read ......'
      return
      end if
C
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

      call msazrecvpart(knode,kdgof,ncoor,numtyp,numnode,
     *numblk,iblk,nsource,num,nne,nummat,mmat,maxrtemp,maxitemp,
     *idisp1,idisp2,t0,tmax,dt,
     *aa(kna0),aa(kna1),aa(kna2),aa(kna3),aa(kna4),
     *aa(kna5),aa(kna6),aa(kna7),
     *ia(knb0),ia(knb1),ia(knb2),ia(knb3),ia(knb4),
     *filename)
 
      close(1)
      close(30)
      close(31)
      close(32)
      close(33)
      close(35)
      close(36)
      close(37)
      close(38)
      return
      end
      
      
      subroutine msazrecvpart(knode,kdgof,ncoor,numtyp,numnode,
     *numblk,iblk,nsource,num,nne,nummat,mmat,maxrtemp,maxitemp,
     *idisp1,idisp2,t0,tmax,dt,
     *u0,u1,u2,coor,bfu,
     *emate,bfd,rtemp,
     *node,nodvar,nodeall,nid,
     *itemp,
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
c
c.......open and read coor0 file
c
        read (30) ((coor(i,j),i=1,ncoor),j=1,knode)
c
c
      ISTATUS = 0
      call openf(31,2,ISTATUS)
      call initrwf(31,iui,iur)
      iui = iui+1
      ipool(iui)=knode
      iui = iui+1
      ipool(iui)=ncoor
      call endrwf(31,iui,iur)
c       write(31) ((coor(i,j),i=1,kcoor),j=1,knode)
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
c      if(iblk.eq.0) then
c        write(*,*) 'coor0 ===knode,ncoor=',knode,ncoor
c        write(*,*)
c        do ii = 1, knode
c        write(*,1100) ii,(coor(j,ii),j=1,ncoor)
c        end do
c      end if
c
c
c.......open and read id0 file
c
      read (31) ((nodvar(I,J),I=1,kdgof),J=1,knode)
c
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
c      if(iblk.eq.0) then
c        write(*,*) 'id0 ===knode,kdgof=',knode,kdgof
c        write(*,*)
c        do ii = 1, knode
c        write(*,1000) ii,(nodvar(j,ii),j=1,kdgof)
c        end do
c      end if
c
C
C.......open and read disp0 file
c
      read (32) nskip,nskip
      read (32) ((bfu(i,j),i=1,kdgof),j=1,knode)
c
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
c     if(iblk.eq.0) then
C        write(*,*) 'disp0 ===knode,kdgof=',knode,kdgof
C        write(*,*)
C        do i = 1, knode
C        write(*,1000) i,(bfu(j,i),j=1,kdgof)
C        end do
c     end if
c  
C
C.......open and read disp1 file
      read (33) nskip,nskip
      read (33) ((u0(i,j),i=1,kdgof),j=1,knode)
c
      
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
c
c     if(iblk.eq.0) then
C        write(*,*) 'disp1 ===knode,kdgof=',knode,kdgof
C        write(*,*)
C        do i = 1, knode
C        write(*,1000) i,(u0(j,i),j=1,kdgof)
C        end do
c     end if
c
c
c...  read etype0 file
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
c     if(iblk.eq.0) then
C      write(*,*) 'Element information =numnod,nummat=',numnod,nummat
C      write(*,*)
C      do i = 1, numtyp
C      write(*,*) i,idet(i),numa(i),nnea(i),mmta(i),nmta(i)
C      end do
c     end if
c              
c
c ... read elem0 file
      read(35) (node(i),i=1,num*nne)
      read(35) nummat,mmat
      read(35) (emate(i),i=1,nummat*mmat)
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
c
c
c
c 
c......receive index of subdomain gross equation number ... ...
c
       read(36) nskip
       read(36) (nodeall(i),i=1,knode)
       read(36) (nid(i),i=1,knode)
c
c
      ISTATUS = 0
      call openf(34,8,ISTATUS)

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
      
      
c      	write(*,*) (nodeall(i),i=1,knode)
c      	write(*,*) (nid(i),i=1,knode)
      
c
c       if(iblk.eq.0) then
c        write(*,*) 'Gross index nodeall===knode=',knode
c        write(*,*)
c        do ii = 1, knode
c        write(*,1000) ii,nodeall(ii)
c        end do
c       end if
c
c      if(iblk.eq.0) then
c      write(*,*) ' subnodes '
c      write(*,*) knode,kdgof
c      do i=1,knode
c      write(*,*) (nodvarall(j,i),j=1,kdgof)
c      end do
c
c      open(56,file='subnodes_check',status='unknown',form='formatted')
c      write(56,*) knode,kdgof
c      do i=1,knode
c      write(56,*) (nodvarall(j,i),j=1,kdgof)
c      end do
c      close(56)
c      end if
c
c
c.......OPEN AND read DISP2 FILE
c
      if (idisp1.eq.1) then
      read (37) nskip,nskip
      read (37) ((u1(i,j),i=1,kdgof),j=1,knode)
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
c     if(iblk.eq.0) then
C        write(*,*) 'disp2 ===knode,kdgof=',knode,kdgof
C        write(*,*)
C        do i = 1, knode
C        write(*,1000) i,(u1(j,i),j=1,kdgof)
C        end do
c     end if
c
      end if
c
C.......OPEN AND read DISP3 FILE
c
      if (idisp2.eq.1) then
      read (38) nskip,nskip
      read (38) ((u2(i,j),i=1,kdgof),j=1,knode)
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
c     if(iblk.eq.0) then
C        write(*,*) 'disp3 ===knode,kdgof=',knode,kdgof
C        write(*,*)
C        do i = 1, knode
C        write(*,1000) i,(u2(j,i),j=1,kdgof)
C        end do
c     end if
      end if
c
c
c
c     read and check the block information data ... ...
c
      read(1) numblk_r,numtyp_r,numnode_r,knode_r,kdgof_r,
     & num_r,nne_r,nummat_r,mmat_r,ncoor_r,idisp1_r,idisp2_r,
     & t0_r,tmax_r,dt_r
c   
      if(numblk_r.ne.numblk) then
      write(*,*) 'Error, unmatch parameter numblk ... ...'
      return
      end if
c
      if(numtyp_r.ne.numtyp) then
      write(*,*) 'Error, unmatch parameter numtyp ... ...'
      return
      end if
c
      if(knode_r.ne.knode) then
      write(*,*) 'Error, unmatch parameter knode ... ...'
      return
      end if
c
      if(knode_r.ne.knode) then
      write(*,*) 'Error, unmatch parameter knode ... ...'
      return
      end if
c
      if(kdgof_r.ne.kdgof) then
      write(*,*) 'Error, unmatch parameter kdgof ... ...'
      return
      end if
c
      if(num_r.ne.num) then
      write(*,*) 'Error, unmatch parameter num ... ...'
      return
      end if
c
      if(nummat_r.ne.nummat) then
      write(*,*) 'Error, unmatch parameter nummat ... ...'
      return
      end if
c
      if(ncoor_r.ne.ncoor) then
      write(*,*) 'Error, unmatch parameter ncoor ... ...'
      return
      end if
c
      if(idisp1_r.ne.idisp1) then
      write(*,*) 'Error, unmatch parameter idisp1 ... ...'
      return
      end if
c
      if(idisp2_r.ne.idisp2) then
      write(*,*) 'Error, unmatch parameter idisp2 ... ...'
      return
      end if
c
1000    format(10i10)
1100    format(i6,8e16.5)
1200    format(6i10)
        return
        end
 
