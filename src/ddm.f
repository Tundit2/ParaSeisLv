C****************************************************************************C
C              & & &     & & & &    & & & &    & & &      & & & &            C
C              &    &    &          &          &    &    &                   C
C              & & &     & & & &    & & & &    & & &     &     & &           C
C              &         &          &          &         &      &            C
C              &         &          & & & &    &           && & &            C
C                                                                            C
C     0216 - 2007 (C) COPYRIGHT GRADUATE UNIVERSITY OF                       C
C                               CHINESE ACADEMY OF SCIENCES.                 C
C                     All Rights Reserved                                    C
C                     AUTHORS:                                               C
C                       Qiu Meng                                             C
C                       Huai Zhang *                                         C
C                       Email:  hzhang@gucas.ac.cn                           C
C                               qmeng@whu.edu.cn                             C
C****************************************************************************C
C ---------------------------------------------------------------------------C
C ddm.f - Parallelized Fortran Version                                       C
C FILE: ddm.f                                                                C
C ddm.f         : main program, based on Lagrange Multiplier Domain          C
C                 Decomposition Method. (LMDDM)                              C
C ddmm.f:       : master subroutines of preconditionered Krylov Subspace     C
C                 iteration method.                                          C
C ddms.f:       : slave subroutines for this software packages.              C
C sendpart.f    : send all the initial data from master node to slave node   C
C recvpart.f    : slave receive data for master node                         C
C                 index.                                                     C
C mreadpart.f   : master node read its partition data for computing.         C
C bft.f         : boundary values and initial values update for each time.   C
C start.f       : initial node id and initial values and boundary values.    C
C dmbsdiag.f    : caculate the graph structure of distributed one-dimension  C
C                 compressed sparse row gross stiff matrix.                  C
C dmbs2dmsr.f   : transfer the DMBS distributed gross matrix format to DMSR  C
C                 distributed gross matrix format.                           C
C azsolv.f      : call Aztec subroutine.                                     C
C e*.f          : element caculate subroutines for all types of meshes.      C
C u*.f          : postprocessing subroutines for each subdomain.             C
C                                                                            C
C ---------------------------------------------------------------------------C
C                                                                            C
C Partition programs:                                                        C
C                                                                            C
C ddmpartition.f: mesh partition based on Metis 4.0 packages.                C
C csrgraphpart.f: generate unstructured sparse graph from hyberid finite     C
C                 element meshes, partition this graph using K-way graph     C
C                 partitioning method.                                       C
C datapart.f    : data partition according to the results of mesh partition  C
C                 or sparse graph partition.                                 C
C ---------------------------------------------------------------------------C
C                                                                            C
C Library subroutines and head files:                                        C
C                                                                            C
C fepglib.f     : lib subroutines for fepg                                   C
C files.f       : transfer harddisk file to memory, using file name and type C
C swap.f        : managing the swap space of each subroutines, using dummy   C
C                 arguments.                                                 C
C timer.f       : timer of computing time and overheads.                     C
C partitionsub.f: subroutines of mesh partition, graph partition and data    C
C                 partition.                                                 C
C files.h       : head file for files.f                                      C
C swap.h        : head file for swap.f                                       C
C ---------------------------------------------------------------------------C
C                                                                            C
C Library subroutines for Message Passing Interface(MPI):                    C
C                                                                            C
C inimpi.f      : initialize the MPI communication environment.              C
C intcommu.f    : MPI communication subroutines for all types of integer     C
C                 data.                                                      C
C realcommu.f   : MPI communication subroutines for all types of double      C
C                 precision data.                                            C
C charcommu.f   : MPI communication subroutines for character data.          C
C                                                                            C
C ---------------------------------------------------------------------------C
C                                                                            C
C C library files:                                                           C
C                                                                            C
C subtime.c     : wall timer.                                                C
C partdmesh.c   : modified Metis 4.0 program file to C function file, called C
C                 by ddmpartition.f.                                         C
C io.c          : called by partdmesh.c file, read mesh file.                C
C kmetis.c      : modified Metis 4.0 program file to C function file, called C
C                 by ddmpartition.f.                                         C
C kmetisio.c    : called by csrgraphpart.f, read sparse graph file.          C
C                                                                            C
C ---------------------------------------------------------------------------C
C Explanation of constants and variables                                     C
C   MAXWORKER                      =  maximum number of workers tasks        C
C   MINWORKER                      =  minimum number of workers tasks        C
C   NWORKERS                       =  number of workers processes            C
C   NDEST,NSOURCE                  =  to - from for message send-receive     C
C ---------------------------------------------------------------------------C
C                                                                            C
C Notice:                                                                    C
C                                                                            C
C  In this parallelized version, the grid is decomposed by the master        C
C process and then mapped and distributed to workers processes.              C
C                                                                            C
C ---------------------------------------------------------------------------C
      program ddm
      implicit double precision (a-h,o-z)
      character*20 fname,sname,filename(20)
      include 'mpif.h'
      include 'memalloc.h'
      common /pool/ rpool(maxrpoolm),ipool(maxrpoolm)
      common /aa/ aa(maxaa)
      common /ia/ ia(maxia)
      INTEGER :: cr
      CALL system_clock(count_rate=cr)
      rate = REAL(cr)

c     Initializing the parallel computation encirenment and find out
c     how many nodes this programm can use,if the number of the
c     processes is less than the minium number if processes assigned,
c     then quit........
      master = 0
      maxnodeid = 255
      minnodeid = 2 
      maxstep = 59400
      write(*,*) 'initializing the parallel computational environment..'
c      integer         :: int1,int2
      call inimpi(myrank,numblk)
      call initpool
      call initimer(ierr)
      
      itter = 1
c      print *,'initimer   ok...................',iblk
c      print *,'myrank=====',myrank

      iblk = myrank
      if (iblk.eq.0) then
      call system_clock(int1)
      int0 = 0
      end if

      if (myrank.eq.master) then
      print *, '*********** Starting Master Node Programing *********'
c      call Mazpartition
      call Mazsendpart
      print *,'send azsendpart    ok...................'
      call Mmsazrecvpart(iblk)
      print *,'msazrecvpart       ok...................'
      else
c
      print *, '*********** Starting Slave Nodes Programing *********'
      call Mazrecvpart(iblk)
      print *,'azrecvpart ok...................',iblk
      end if
c
c      call timer(9,1)

      istep = 0
      call Mstart(iblk,itter)
      print *,'start      ok...................',iblk
9999   continue
      istep = istep+1
      Call Mbft(iblk)
c      print *,'bft        ok...................',iblk
c      itter = 0
8888   continue
c      itter = itter + 1
      call Mesddm(iblk)
c      print *,'eddm       ok...................',iblk

      call Mgather(iblk,maxstep,istep,icontinue)
      if (icontinue.eq.1) goto 9999

      if (iblk.eq.0) then
      call system_clock(int2)
      print*, 'The total runtime is ' , (int2-int1)/rate , 'sec'
      end if
1212  call MPI_FINALIZE
      end

      
      
      
      
