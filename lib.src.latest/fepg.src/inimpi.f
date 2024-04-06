c******************************************************************************c
c FILE: inimpi.f                                                               c
c Author: Huai Zhang                                                           c
c DESCRIPTION:                                                                 c
c  Called by ddm.f code to obtain message passing interface(MPI) environment   c
c  and initialize the user level communication protocol, then return the rank  c
c  of the distributed parallel machine node to mail program.                   c 
c******************************************************************************c
      subroutine inimpi(myrank,numblk)

      implicit real*8 (a-h,o-z)
      common/messg/numcpus,myid,ndomains,msids(2,0:256),msidm(2),iprint,
     &             mycomm
      logical filexist
      include 'mpif.h'
c
c     initial message common block
c
      master = 0
      nsource = 0
      do i=1,256
      msidm(i) = 0
      do j = 1,2
      msids(j,i) = 0
      end do
      end do
      iprint = 0 
      call MPI_INIT( ierr )
      call MPI_COMM_DUP( MPI_COMM_WORLD, mycomm, ierr)
      call MPI_COMM_RANK( mycomm, myid, ierr )
      call MPI_COMM_SIZE( mycomm, numnodes, ierr )
      numcpus = numnodes
      myrank = myid
      if (myid .eq. master) then
      write(*,*) '**********  Starting MPI Master Task  ************'
c
c     get the lmddm control file
c
      numblk = numcpus - 1
      inquire(file='partition.dat',exist=filexist)
      if(.not.filexist) then
       open(21,file='partition.dat',form='formatted',status='unknown')
       write(21,*) numblk,1
       write(21,*) 1,1,numblk
       close(21)
      else
       open(21,file='partition.dat',form='formatted',status='unknown')
       read(21,*) numb,isgn
       read(21,*) nx,ny,nz
       close(21)
       if(isgn.eq.2) numb = nx*ny*nz
       if(numb.ne.numblk) then
        print *,'numblk not matched nprocess ! Check partition.dat ..'
        call endjob(ierr)
       endif
      endif
c      write(*,*) numcpus,' CPUs compute ',numblk,' subdomains.'//
c     +           ' Node ',myid,' is assigned as master node.'//
c     +           ' and the other nodes are all assigned as slave nodes.'
c
c     send out numblk parameter 
c
      do iblk = 1,numblk
      call sendint(iblk,nsource,numblk)
      end do
c
      end if  ! master node
c
c     slave node task 
c
      if(myid.gt.master) then
c
c     receive numblk from master node
c
      call recvint(myid,nsource,numblk)
c
c     data check for node balancing 
c
      ndomains = numblk
      if((myid).gt.numblk) then  
      write(*,*) 'There are more CPUs than blocknumbers of subdomains'
      write(*,*) 'Fatal error ............'
      call endjob(ierr)
      end if
      if(myid.gt.255) then
      write(*,*) ' CPUs number greater then 255, stop......'
      call endjob(ierr)
      end if
      if(numblk.gt.255) then
      write(*,*) ' subdomain number greater then 255, stop......'
      call endjob(ierr)
      end if 
c
c     Current version must has condition as following:
c
      if (numcpus.ne.(numblk+1)) then
      write(*,*)' There are less CPUs to compute all the subdomains...'
      call endjob(ierr)
      end if
c
      end if ! slave node
c  
      return
      end

      subroutine findworkers(numworkers)
      implicit real*8 (a-h,o-z)
      common/messg/numcpus,myid,ndomains,msids(2,0:256),msidm(2),iprint,
     &              mycomm
      integer  ierr,numworkers
      include 'mpif.h'
c
      call MPI_COMM_SIZE( mycomm, numcpu, ierr )
      numworkers = numcpu-1
      if (numworkers.le.0) then
      print *,'Number_workers less than Minworkers,fatal errer!!'
      call endjob(ierr)
      end if
      return
      end

      subroutine findrank(numrank)
      implicit real*8 (a-h,o-z)
      common/messg/numcpus,myid,ndomains,msids(2,0:256),msidm(2),iprint,
     &              mycomm
      integer  ierr,numrank
      include 'mpif.h'
c
      call MPI_COMM_RANK( mycomm, myrank, ierr )
      numrank = myrank
      if (numrank.le.0) then
      print *,'Number_rank less than Minworkers,fatal errer!!'
      call endjob(ierr)
      end if
      return
      end

      subroutine endjob(ierr)
      implicit real*8 (a-h,o-z)
      common/messg/numcpus,myid,ndomains,msids(2,0:256),msidm(2),iprint,
     &              mycomm
      integer  ierr,numrank
      include 'mpif.h'
c
      call MPI_COMM_FREE(mycomm, ierr)
      call MPI_FINALIZE(ierr)
      print *,'The process has been successfully finalized !!'
      return
      end

      subroutine abortjob(ierr)
      implicit real*8 (a-h, o-z)
      common/messg/numcpus,myid,ndomains,msids(2,0:256),msidm(2),iprint,
     &              mycomm
      integer ierr,ierrcode
      include 'mpif.h'
c
      call MPI_ABORT(mycomm, ierrcode, ierr)
      call MPI_COMM_FREE(mycomm, ierr)
      call MPI_FINALIZE(ierr)
      print *,"The process has been stopped with some error!!"
      return
      end
