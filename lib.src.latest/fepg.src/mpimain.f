C**************************************************************************C
C              & & &     & & & &    & & & &    & & &      & & & &          C
C              &    &    &          &          &    &    &                 C
C              & & &     & & & &    & & & &    & & &     &     & &         C
C              &         &          &          &         &      &          C
C              &         &          & & & &    &           && & &          C
C                                                                          C
C     1026 - 2002 (C) COPYRIGHT INISTITUTE OF MATHEMATICS,                 C
C                               CHINESE ACADEMY OF SCIENCES.               C
C                     All Rights Reserved                                  C
C                     AUTHORS:                                             C
C                       Guoping Liang                                      C
C                       Email:  guoping@fegensoft.com                      C
C                               ling@fegensoft.com                         C
C                       Huai Zhang                                         C
C                       Email:  huaizhang@hotmail.com                      C
C                               hzhang@mail.amss.ac.cn                     C
C                       Shaopeng Liu                                       C
C                       Email:                                             C
C                       Date :  Oct. 2002                                  C
C**************************************************************************C
C -------------------------------------------------------------------------C
      program ddm
      implicit real*8 (a-h,o-z)
      include 'mpif.h'
c     Initializing the parallel computation encirenment and find out
c     how many nodes this programm can use,if the number of the
c     processes is less than the minium number if processes assigned,
c     then quit........
      master = 0
      maxnodeid = 255
      minnodeid = 2 
      write(*,*) 'initializing the parallel computational environment..'
      call initimer(ierr)
      call inimpi(myrank,numblk)
      call timer(9,1)
      if (myrank.eq.master) then
c      print *, '*********** My node rank is:',myrank,'***************'
c      print *, '*********** Starting Master Node Programing *********'
      call masterp
c      write(*,*)'.......... Master Program Success !!! ..............'
      else
c      print *, '*********** My node rank is:',myrank,'***************'
c      print *, '*********** Starting Slave Nodes Programing *********'
      call slavep(myrank)
c      write(*,*)'.......... Slave Program Success !!! ..............'
      endif
c 
c     time and load imbalance report and analysis 
c
      call timer(6,1)
      call timer(7,1)
      call timer(8,1)
      call timer(9,2)
      if (myrank.eq.master) then
      open(88,file='report',form='formatted', status='unknown')
      end if
      ioport = 88
      isreport = 1
      call  timereport(myrank,myrank,numblk,ioport,isreport)
      if (myrank.eq.master) then
      close(88)
      end if 
c
      call endjob(ierr)
c
c
      stop
      end

