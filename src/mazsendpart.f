      subroutine Mazsendpart
      implicit real*8 (a-h,o-z)
      character*12 fname,filename(20)
      character*12 fname1,fname2
      include 'memalloc.h'
      logical filflgdisp(20)
      common /pool/ rpool(maxrpoolm),ipool(maxrpoolm)
      common /aa/ aa(maxaa)
      common /ia/ ia(maxia)
c
      nsource=0
c
      idisp1 = 0
      idisp2 = 0
c      print*, 'start read mlmddm ...'
      open(1,file='mlmddm',form='unformatted',status='unknown')
      read(1) numblk,numtyp,numnode,knode,kdgof,num,nne,nummat,mmat,
     &   ncoor,idisp1,idisp2,t0,tmax,dt
      close(1)
c      print*, 'mlmddm read finished'
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
      do 1000 iblk=0,numblk-1
c      	print*, 'Start send iblk',iblk
      read (30) knode,ncoor
      read (31) knode,kdgof
      read (35) num,nne
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
      kna7=maxaa*1
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
      kna7=maxaa - kna6
      maxrtemp = kna7
c
      knb0=1
      knb1=knb1+knb0
      knb2=knb2+knb1
      knb3=knb3+knb2
      knb4=knb4+knb3
      knb5=maxia-knb4
      maxitemp=knb5
      
c      write(*,*) 'Maxitemp, maxrtemp ===',maxitemp, maxrtemp 

      call azsendpart(knode,kdgof,ncoor,numtyp,numnode,
     *numblk,iblk,nsource,num,nne,nummat,mmat,maxrtemp,maxitemp,
     *idisp1,idisp2,t0,tmax,dt,
     *aa(kna0),aa(kna1),aa(kna2),aa(kna3),aa(kna4),
     *aa(kna5),aa(kna6),
     *ia(knb0),ia(knb1),ia(knb2),ia(knb3),ia(knb4),
     *filename)
 
1000  continue
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
      
      
      subroutine azsendpart(knode,kdgof,ncoor,numtyp,numnode,
     *numblk,iblk,nsource,num,nne,nummat,mmat,maxrtemp,maxitemp,
     *idisp1,idisp2,t0,tmax,dt,
     *u0,u1,u2,coor,bfu,
     *emate,rtemp,
     *node,nodvar,nodeall,nid,itemp,
     *filename)
      implicit real*8 (a-h,o-z)
      character*12 filename(20)
        include 'memalloc.h'
      common /pool/ rpool(maxrpoolm),ipool(maxrpoolm)
      dimension u0(kdgof,knode),u1(kdgof,knode),u2(kdgof,knode),
     & coor(ncoor,knode),bfu(kdgof,knode),
     & emate(nummat*mmat),rtemp(maxrtemp)
       dimension  nodvar(kdgof,knode),nodeall(knode),nid(knode),
     *  itemp(maxitemp)
       dimension node(num*nne)
c
       logical filflgdisp(20)
c
c
c ... read elem0 file
      read(35) (node(i),i=1,num*nne)
      read(35) nummat,mmat
      read(35) (emate(i),i=1,nummat*mmat)
c

c
c.......open and read coor0 file
c
        read (30) ((coor(I,J),i=1,ncoor),j=1,knode)

c
c.......open and read id0 file
c
      read (31) ((nodvar(I,J),I=1,kdgof),J=1,knode)
c     if(iblk.eq.8) then
C        write(*,*) 'id0 ===knode,kdgof=',knode,kdgof
C        write(*,*)
C        do i = 1, knode
C        write(*,1000) i,(nodvar(j,i),j=1,kdgof)
C        end do
c     end if
C
C.......open and read disp0 file
c
      read (32) nskip,nskip
      read (32) ((bfu(i,j),i=1,kdgof),j=1,knode)
c

C
C.......open and read disp1 file
      read (33) nskip,nskip
      read (33) ((u0(i,j),i=1,kdgof),j=1,knode)
C
C
C
C........read index of subdomain nodes file
       read (36) knodee
       read(36) (nodeall(i),i=1,knodee)
       read(36) (nid(i),i=1,knodee)
c
c.......OPEN AND read DISP2 FILE
c
      if (idisp1.eq.1) then
      read (37) nskip,nskip
      read (37) ((u1(i,j),i=1,kdgof),j=1,knode)
C
c     if(iblk.eq.8) then
C        write(*,*) 'disp2 === knode,kdgof=',knode,kdgof
C        write(*,*)
C        do i = 1, knode
C        write(*,1000) i,(u1(j,i),j=1,kdgof)
C        end do
c     end if
      end if
c
C.......OPEN AND read DISP3 FILE
c
      if (idisp2.eq.1) then
      read (38) nskip,nskip
      read (38) ((u2(i,j),i=1,kdgof),j=1,knode)
C
c     if(iblk.eq.8) then
C        write(*,*) 'disp3 === knode,kdgof=',knode,kdgof
C        write(*,*)
C        do i = 1, knode
C        write(*,1000) i,(u2(j,i),j=1,kdgof)
C        end do
c     end if
      end if
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
      write(*,*) 'Error, unmatch parameter knode ... ...',iblk
c      print*, knode,knode_r
      return
      end if
c
      if(kdgof_r.ne.kdgof) then
      write(*,*) 'Error, unmatch parameter kdgof ... ...'
      return
      end if
c
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
c
c     Send all the partitioned data to slave processes ... ...
c
        if(iblk.eq.0) return  ! aviod self-communication ... ...
C
C     write LMDDM FILE FOR EACH SUBDOMAIN
C
C      write(40)numblk,numtyp,maxlm,nmdof,numtypl,kdgofl,keymt,
C     & idisp1,idisp2
C      write(40) lgio,t0,tmax,dt
c      
      call sendint(iblk,nsource,numblk)
      call sendint(iblk,nsource,numtyp)
      call sendint(iblk,nsource,numnode)
      call sendint(iblk,nsource,knode)
      call sendint(iblk,nsource,kdgof)
      call sendint(iblk,nsource,num)
      call sendint(iblk,nsource,nne)
      call sendint(iblk,nsource,nummat)
      call sendint(iblk,nsource,mmat)
      call sendint(iblk,nsource,ncoor)
      call sendint(iblk,nsource,idisp1)
      call sendint(iblk,nsource,idisp2)
      call sendr(iblk,nsource,t0)
      call sendr(iblk,nsource,tmax)
      call sendr(iblk,nsource,dt)
c
C
C      Send out the coor0 file
C
C      write(41) knode,ncoor
C      write(41) ((coor(i,j),i=1,ncoor),J=1,knode)
        call sendint(iblk,nsource,knode)
        call sendint(iblk,nsource,ncoor)
        iur = 0
        iur0=iur+1
        do j=1,knode
        do i=1,ncoor
        iur = iur+1
        rtemp(iur)=coor(i,j)
        enddo
        enddo
        call sendar(iblk,nsource,rtemp(iur0),knode*ncoor)
C
C     send out the ID0 file
C
c     write(42) knode,kdgof
c     write(42) ((nodvar(i,j),i=1,kdgof),j=1,knode)
c
        call sendint(iblk,nsource,knode)
        call sendint(iblk,nsource,kdgof)
        iui = 0
        iui0=iui+1
        do j=1,knode
        do i=1,kdgof
        iui = iui+1
        itemp(iui)=nodvar(i,j)
        enddo
        enddo
        call sendai(iblk,nsource,itemp(iui0),knode*kdgof)
C
C     Send out the constrained boundary file (disp0) data 
C
c     write(43) knode,kdgof
c     write(43) ((bfu(i,j),i=1,kdgof),j=1,knode)
        call sendint(iblk,nsource,knode)
        call sendint(iblk,nsource,kdgof)
        iur = 0
        iur0=iur+1
        do j=1,knode
        do i=1,kdgof
        iur = iur+1
        rtemp(iur)=bfu(i,j)
        enddo
        enddo
        call sendar(iblk,nsource,rtemp(iur0),knode*kdgof)
C
C    send out the disp1 file (Initial values of development equations ... ...)
C
c    write(44) knode,kdgof
c    write(44) ((u0(i,j),i=1,kdgof),j=1,knode)
        call sendint(iblk,nsource,knode)
        call sendint(iblk,nsource,kdgof)
        iur = 0
        iur0=iur+1
        do j=1,knode
        do i=1,kdgof
        iur = iur+1
        rtemp(iur)=u0(i,j)
        enddo
        enddo
        call sendar(iblk,nsource,rtemp(iur0),knode*kdgof)
C
c
C       send out elem0 file
C
c        write(46) (nodeall(i),i=1,nodall)
c        write(46) (emateall(i),i=1,matall)
       call sendint(iblk,nsource,num)
       call sendint(iblk,nsource,nne)
       call sendint(iblk,nsource,nummat)
       call sendint(iblk,nsource,mmat)
c
       iui = 0
       iur = 0
       iui0=iui+1
       iur0=iur+1
       do i=1,num*nne
       iui = iui+1
       itemp(iui)=node(i)
       enddo
       call sendai(iblk,nsource,itemp(iui0),1*num*nne)
       iui = 0
       iur = 0
       iui0=iui+1
       iur0=iur+1
       do i=1,nummat*mmat
       iur = iur+1
       rtemp(iur)=emate(i)
       enddo
       call sendar(iblk,nsource,rtemp(iur0),1*nummat*mmat)
c 
c......send out index of subdomain gross equation number ... ...
c
c       write(47) knode,kdgof,
        call sendint(iblk,nsource,knode)
c
        iui = 0
        iur = 0
        iui0=iui+1
        iur0=iur+1
        do i=1,knode
        iui = iui+1
        itemp(iui)=nodeall(i)
        enddo
        call sendai(iblk,nsource,itemp(iui0),1*knode)
        
        iui = 0
        iur = 0
        iui0=iui+1
        iur0=iur+1
        do i=1,knode
        iui = iui+1
        itemp(iui)=nid(i)
        enddo
        call sendai(iblk,nsource,itemp(iui0),1*knode)
        
c      if(iblk.eq.2) then
c      	write(*,*) knode
c      	write(*,*) (nodeall(i),i=1,knode)
c      end if
        
C
C     send out DISP2 for INITIAL DU/DT
C
      if (idisp1.eq.1) then
c     write(48)knode,kdgof
c     write(48)((u1(i,j),i=1,kdgof),j=1,knode)
        call sendint(iblk,nsource,knode)
        call sendint(iblk,nsource,kdgof)
        iui = 0
        iur = 0
        iui0=iui+1
        iur0=iur+1
        do j=1,knode
        do i=1,kdgof
        iur = iur+1
        rtemp(iur)=u1(i,j)
        enddo
        enddo
        call sendar(iblk,nsource,rtemp(iur0),knode*kdgof)
        endif
C
C     send out DISP3 FOR INITIAL (DU/DT)2
C
      if (idisp2.eq.1) then
c     write(49)knode,kdgof
c     write(49)((u2(i,j),i=1,kdgof),j=1,knode)
        call sendint(iblk,nsource,knode)
        call sendint(iblk,nsource,kdgof)
        iui = 0
        iur = 0
        iui0=iui+1
        iur0=iur+1
        do j=1,knode
        do i=1,kdgof
        iur = iur+1
        rtemp(iur)=u2(i,j)
        enddo
        enddo
        call sendar(iblk,nsource,rtemp(iur0),knode*kdgof)
        endif

c
1000    format(10i10)
1100    format(i6,8e16.5)
1200    format(6i10)
      return
      end
 
 
