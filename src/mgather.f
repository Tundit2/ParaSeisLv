      subroutine Mgather (iblk,maxstep,istep,icontinue)
      implicit real*8 (a-h,o-z)
      character*12 fname,filename(20)
      character*12 fname1,fname2
      include 'partdata.h'
      logical filflgdisp(20)
      include 'memalloc.h'
      common /pool/ rpool(maxrpools),ipool(maxrpools)
      common /aa/ aa(maxaa)
      common /ia/ ia(maxia)
      
      icontinue = 1
      if (istep.gt.maxstep) then
        icontinue = 0
        return
      endif
      
      nsource = 0
      master = 0
      
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
      
      kna1 = knode*kdgof
      kna2 = knode*kdgof
      kna3 = numnode*kdgof
      kna4 = numnode*kdgof
      
      knb1 = knode
      knb2 = knode
      knb3 = numnode
      
      kna0 = 1
      kna1 = kna1+kna0
      kna2 = kna2+kna1
      kna3 = kna3+kna2
      kna4 = kna4+kna3
      kna5 = maxaa-kna4
      maxrtemp = kna5
      
      knb0 = 1
      knb1 = knb1+knb0
      knb2 = knb2+knb1
      knb3 = knb3+knb2
      knb4 = maxia-knb4
      maxitemp = knb4
      
      if (iblk.eq.master) then
      
      call mgatheru(numblk,knode,kdgof,numnode,iblk,
     *  maxrtemp,maxitemp,
     *  aa(kna0),aa(kna1),aa(kna2),aa(kna3),aa(kna4),
     *  ia(knb0),ia(knb1),ia(knb2),ia(knb3))
      
      else
      call sgatheru(numblk,knode,kdgof,numnode,iblk,
     *  aa(kna0),aa(kna1),
     *  ia(knb0),ia(knb1))
      
      end if
      
      return
      end 
      
      subroutine mgatheru(numblk,knode,kdgof,numnode,iblk,
     *  maxrtemp,maxitemp,
     *  eu,ev,u,v,rtemp,
     *  nodeall,nid,ifill,itemp)
      implicit real*8 (a-h,o-z)
      character*12 filename
      include 'memalloc.h'
             include 'mpif.h'
       dimension nstat(MPI_STATUS_SIZE)
      common /pool/ rpool(maxrpools),ipool(maxrpools)
      dimension  eu(kdgof,knode),ev(kdgof,knode),
     * u(kdgof,numnode),v(kdgof,numnode),
     * nodeall(knode),nid(knode),ifill(numnode),
     * itemp(maxitemp),rtemp(maxrtemp)
      
      if (iblk.ne.0) then
      	print*, 'gather wrong!'
      	return
      end if
      
      master = 0
      
      
c     initial the ifill maxtrix
      do i = 1,numnode
      	ifill(i) = 0
      end do
c      
      ISTATUS = 4
      call openf(21,4,ISTATUS)
      call initrwf(21,iui,iur)
      iui = iui+1
      knode = ipool(iui)
      iui = iui+1
      kdgof = ipool(iui)
      call endrwf(21,iui,iur)
      call initrwf(21,iui,iur)
      do j=1,kdgof
      	do i = 1,knode
      		iur = iur+1
      		eu(j,i) = rpool(iur)
      	end do
      end do
      
      ISTATUS = 5
      call openf(33,5,ISTATUS)
      call initrwf(33,iui,iur)
      do j = 1,kdgof
      	do i = 1,knode
      		iur = iur+1
      		ev(j,i) = rpool(iur)
      	end do
      end do
      
      ISTATUS = 8
      call openf(34,8,ISTATUS)
      call initrwf(34,iui,iur)
      iui = iui+1
      knodee = ipool(iui)
      call endrwf(34,iui,iur)
      call initrwf(34,iui,iur)
      do i = 1,knodee
      	iui = iui+1
      	nodeall(i) = ipool(iui)
      end do
      call endrwf(34,iui,iur)
      call initrwf(34,iui,iur)
      do i = 1,knodee
      	iui = iui+1
      	nid(i) = ipool(iui)
      end do
      call endrwf(34,iui,iur)
      
c      
      do i = 1,knodee
        inod = nodeall(i)
        ij = nid(i)

        ifff = ifill(inod)

        if (ij.eq.1) then
        	if(ifff.eq.1) print*,'FILL CANTMATCH'
        	do j = 1,kdgof
        		u(j,inod) = eu(j,i)
        		v(j,inod) = ev(j,i)
        	end do
        	ifill(inod) = 1

        end if
      end do
      
      
      
      do ipart = 1,numblk-1
      	nsource = ipart
      	messagetag = 999+ipart
      	call MPI_RECV(knode1,1,MPI_INTEGER,nsource,messagetag,
     &               MPI_COMM_WORLD,nstat,ierr)
        messagetag = 989+ipart
        call MPI_RECV(kdgof1,1,MPI_INTEGER,nsource,messagetag,
     &               MPI_COMM_WORLD,nstat,ierr)
c      	call recvint(iblk,nsource,knode1)
c      	call recvint(iblk,nsource,kdgof1)
c      	print*, '***',knode1,kdgof1,ipart
      	messagetag = 979+ipart
      	ki = 1
      	call MPI_RECV(itemp(ki),knode1,MPI_INTEGER,
     &             nsource,messagetag,MPI_COMM_WORLD,nstat,ierr)
        ki = ki+knode1
        messagetag = 969+ipart
        call MPI_RECV(itemp(ki),knode1,MPI_INTEGER,
     &             nsource,messagetag,MPI_COMM_WORLD,nstat,ierr)
        kr = 1
        messagetag = 959+ipart
        call MPI_RECV(rtemp(kr),knode1*kdgof1,
     &          MPI_DOUBLE_PRECISION,nsource,messagetag,
     &               MPI_COMM_WORLD,nstat,ierr)
        kr = 1+knode1*kdgof1
        messagetag = 949+ipart
        call MPI_RECV(rtemp(kr),knode1*kdgof1,
     &          MPI_DOUBLE_PRECISION,nsource,messagetag,
     &               MPI_COMM_WORLD,nstat,ierr)

      	
      	do i = 1,knode1
        inod = itemp(i)
        ij = itemp(i+knode1)
        ifff = ifill(inod)
        if (ij.eq.1) then
        	if(ifff.eq.1) print*,'FILL CANTMATCH'
        	do j = 1,kdgof1
        		u(j,inod) = rtemp((j-1)*knode1+i)
        		v(j,inod) = rtemp(knode1*kdgof1+(j-1)*knode1+i)
        	end do
        	ifill(inod) = 1

        end if
        end do



      end do
      

      
      do i = 1,numnode
      	ifff = ifill(i)
      	if (ifff.ne.1) print*, 'you must unfill,',i
      end do
      
      do ipart = 1,numblk-1
      	
      	nsource = ipart
      
      	messagetag = 939+ipart
      	call MPI_RECV(knode1,1,MPI_INTEGER,nsource,messagetag,
     &               MPI_COMM_WORLD,nstat,ierr)
        messagetag = 929+ipart
        call MPI_RECV(kdgof1,1,MPI_INTEGER,nsource,messagetag,
     &               MPI_COMM_WORLD,nstat,ierr)
      	messagetag = 919+ipart
      	ki = 1
      	call MPI_RECV(itemp(ki),knode1,MPI_INTEGER,
     &             nsource,messagetag,MPI_COMM_WORLD,nstat,ierr)
      


c      	call recvint(iblk,nsource,knode1)
c      	call recvint(iblk,nsource,kdgof1)
c      	call recvai(iblk,nsource,itemp(1),knode1)
      	
      	do j = 1,kdgof1
      	do i = 1,knode1
      		inod = itemp(i)
      		rtemp((j-1)*knode1+i) = u(j,inod)
      	end do
        end do
 
        
        messagetag = 909+ipart
        call MPI_SEND(rtemp(1),knode1*kdgof1,MPI_DOUBLE_PRECISION,
     &             nsource, messagetag
     &               ,MPI_COMM_WORLD,ierr) 


        
c        call sendar(ipart,master,rtemp(1),knode1*kdgof1)
        
        do j = 1,kdgof1
      	do i = 1,knode1
      		inod = itemp(i)
      		rtemp((j-1)*knode1+i) = v(j,inod)
      	end do
        end do
        messagetag = 899+ipart
        call MPI_SEND(rtemp(1),knode1*kdgof1,MPI_DOUBLE_PRECISION,
     &             nsource, messagetag
     &               ,MPI_COMM_WORLD,ierr) 
        
c        call sendar(ipart,master,rtemp(1),knode1*kdgof1)
        
      end do
      
      
      			
      ISTATUS = 11
      call openf(23,11,ISTATUS)
      call initrwf(23,iui,iur)
      iur = iur+1
      tmax = rpool(iur)
      iur = iur+1
      dt = rpool(iur)
      iur = iur+1
      time = rpool(iur)
      iui = iui+1
      it = ipool(iui)
      call endrwf(23,iui,iur)
      
      ISTATUS = 8
      call openf(34,8,ISTATUS)
      call initrwf(34,iui,iur)
      iui = iui+1
      knode = ipool(iui)
      call endrwf(34,iui,iur)
      call initrwf(34,iui,iur)
      do i = 1,knodee
      	iui = iui+1
      	nodeall(i) = ipool(iui)
      end do
      call endrwf(34,iui,iur)
      
      do i = 1,knode
      	inod = nodeall(i)
      	do j = 1,kdgof
      	  eu(j,i) = u(j,inod)
      	  ev(j,i) = v(j,inod)
      	end do
      end do
      
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
      

      
      print*, 'it.....',it
c      goto 1122
      iit = mod(it,600)
       if (iit .ne.0 ) goto 1122
      write(filename,*) it/600
      print*, filename
      open(10,file='unod.'//trim(adjustl(filename)),
     *  form='formatted',status='unknown')
      write(10, fmt='(E8.3)') ((u(j,i),i=1,numnode),j=1,kdgof) 
      close(10)
      
      open(10,file='unodb.'//trim(adjustl(filename)),
     *  form='formatted',status='unknown')
      write(10, fmt='(E8.3)') ((v(j,i),i=1,numnode),j=1,kdgof)
      close(10)
      
      
c      open(101,file='datunod',
c     *  form='formatted',status='unknown')
c      write(101,*) 'Result of u and v'
c      write(101,*) ((u(j,i),i=1,numnode),j=1,kdgof) 
c      write(101,*) ((v(j,i),i=1,numnode),j=1,kdgof)
c      close(101)
      
      
      
      
      
1122  continue      
      return
      end
      
      subroutine sgatheru(numblk,knode,kdgof,numnode,iblk,
     *  eu,ev,
     *  nodeall,nid)
      implicit real*8 (a-h,o-z)
      character*12 filename(20)
      include 'memalloc.h'
             include 'mpif.h'
       dimension nstat(MPI_STATUS_SIZE)
      common /pool/ rpool(maxrpools),ipool(maxrpools)
      dimension  eu(kdgof*knode),ev(kdgof*knode),
     * nodeall(knode),nid(knode)
      
      master = 0
      
c      
      ISTATUS = 4
      call openf(21,4,ISTATUS)
      call initrwf(21,iui,iur)
      iui = iui+1
      knode = ipool(iui)
      iui = iui+1
      kdgof = ipool(iui)
      call endrwf(21,iui,iur)
      call initrwf(21,iui,iur)
      do j=1,kdgof
      	do i = 1,knode
      		iur = iur+1
      		eu((j-1)*knode+i) = rpool(iur)
      	end do
      end do
      
      ISTATUS = 5
      call openf(33,5,ISTATUS)
      call initrwf(33,iui,iur)
      do j = 1,kdgof
      	do i = 1,knode
      		iur = iur+1
      		ev((j-1)*knode+i) = rpool(iur)
      	end do
      end do
      
      ISTATUS = 8
      call openf(34,8,ISTATUS)
      call initrwf(34,iui,iur)
      iui = iui+1
      knodee = ipool(iui)
      call endrwf(34,iui,iur)
      call initrwf(34,iui,iur)
      do i = 1,knodee
      	iui = iui+1
      	nodeall(i) = ipool(iui)
      end do
      call endrwf(34,iui,iur)
      call initrwf(34,iui,iur)
      do i = 1,knodee
      	iui = iui+1
      	nid(i) = ipool(iui)
      end do
      call endrwf(34,iui,iur)
      
c      
      

      	messagetag = 999+iblk
      	call MPI_SEND(knode,1,MPI_INTEGER,master,messagetag,
     &               MPI_COMM_WORLD,ierr)
        messagetag = 989+iblk
        call MPI_SEND(kdgof,1,MPI_INTEGER,master,messagetag,
     &               MPI_COMM_WORLD,ierr)
        messagetag = 979+iblk
        call MPI_SEND(nodeall(1),knode,MPI_INTEGER,
     &            master,messagetag,MPI_COMM_WORLD,ierr)
        messagetag = 969+iblk
        call MPI_SEND(nid(1),knode,MPI_INTEGER,
     &            master,messagetag,MPI_COMM_WORLD,ierr)
        messagetag = 959+iblk
        call MPI_SEND(eu(1),knode*kdgof,MPI_DOUBLE_PRECISION,
     &             master, messagetag
     &               ,MPI_COMM_WORLD,ierr) 
        messagetag = 949+iblk
        call MPI_SEND(ev(1),knode*kdgof,MPI_DOUBLE_PRECISION,
     &            master, messagetag
     &               ,MPI_COMM_WORLD,ierr) 
        
        
c      	call sendint(master,iblk,knode)
c      	call sendint(master,iblk,kdgof)
c      	print*, '***1',iblk
c      	call sendai(master,iblk,nodeall(1),knode)
c      	call sendai(master,iblk,nid(1),knode)
c      	call sendar(master,iblk,eu(1),knode*kdgof)
c      	call sendar(master,iblk,ev(1),knode*kdgof)
c      	print*, '***2',iblk

        messagetag = 939+iblk
      	call MPI_SEND(knode,1,MPI_INTEGER,master,messagetag,
     &               MPI_COMM_WORLD,ierr)
        messagetag = 929+iblk
        call MPI_SEND(kdgof,1,MPI_INTEGER,master,messagetag,
     &               MPI_COMM_WORLD,ierr)
        messagetag = 919+iblk
        call MPI_SEND(nodeall(1),knode,MPI_INTEGER,
     &            master,messagetag,MPI_COMM_WORLD,ierr)
        
        
c      	call sendint(master,iblk,knode)
c      	call sendint(master,iblk,kdgof)
c      	call sendai(master,iblk,nodeall(1),knode)
      	
      	messagetag = 909+iblk
        call MPI_RECV(eu(1),knode*kdgof,
     &          MPI_DOUBLE_PRECISION,master,messagetag,
     &               MPI_COMM_WORLD,nstat,ierr)
     
        messagetag = 899+iblk
        call MPI_RECV(ev(1),knode*kdgof,
     &          MPI_DOUBLE_PRECISION,master,messagetag,
     &               MPI_COMM_WORLD,nstat,ierr)
      	
      	
  
c        call recvar(iblk,master,eu(1),knode*kdgof)
c        call recvar(iblk,master,ev(1),knode*kdgof)
        
c        goto 1123
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
      		rpool(iur) = eu((j-1)*knode+i)
      	end do
        end do
        call endrwf(21,iui,iur)
      
        ISTATUS = 5
        call openf(33,5,ISTATUS)
        call initrwf(33,iui,iur)
        do j = 1,kdgof
      	do i = 1,knode
      		iur = iur+1
      		rpool(iur) = ev((j-1)*knode+i) 
      	end do
        end do
        call endrwf(33,iui,iur)
      			

      
1123  continue      
      return
      end
