      implicit real*8 (a-h,o-z)
      character*12 fname,filename(20)
      common /aa/ ia(317500000)
c .... open coor0 file
      open(2,file='coor0',
     * form='unformatted',status='old')
      read(2) np,nd
      close(2)
      WRITE(*,*) 'np,nd =',np,nd
      kna1=np*20*2
      knb1=np*nd*2
      knb2=1000000*1
      if (knb2/2*2 .lt. knb2) knb2=knb2+1
      kna0=1
      kna1=kna1+kna0
      knb0=1
      knb1=knb1+knb0
      knb2=knb2+knb1
      call gidpost0(np,nd,ia(kna0),ia(knb0),
     *ia(knb1),
     *filename)
      end
      subroutine gidpost0(np,nd,u,coor,node,
     *filename)
      implicit real*8 (a-h,o-z)
      character*12 filename(20)

      logical filflg
      character row*60,res*5,c*3,mate,field(100)*12,fname*12,
     &         arow(2)*60
      dimension u(np,20),coor(np,nd),node(1000000)
      dimension ndof(100),npdof(100),ipdofno(100,10)
3000  format(1x,i8,6e15.7)
3100  format(1x,i8,28i10)
 
      mate='y'
      inquire(file='material',exist=filflg)
      if (filflg) then
      open(1,file='material',form='formatted',status='old')
      read(1,'(1a)') mate
      close(1)
      endif
 
c .... open gidpost.dof file
      open(2,file='gidpost.dof',form='formatted',status='old')
      read(2,*) ntime,nf
 
c .... open gidpost file
      open(1,file='gidpost',form='formatted',status='old')
 
      do n=1,nf
      read(2,'(8a)') field(n)
cc       write(*,*) 'n & fname ==== ',n,' ',field(n)
      read(2,*) ndof(n),npdof(n),(ipdofno(n,i),i=1,npdof(n))
      enddo
      close(2)
 
c .... open res file
      open(3,file='dzh.flavia.res',form='formatted',status='unknown')
      write(3,*) 'GID Post Results File 1.0'
      write(3,*)
 
      do 2000 n=1,nf
       do j=1,2
        read(1,'(a60)') row
        arow(j)=row
       enddo
      do 1000 it=1,ntime
      WRITE(UNIT=C,FMT='(I3)') it
       fname=field(n)
cc       write(*,*) 'n & fname ==== ',n,' ',fname
c       do i=1,12
c        if (fname(i:i).eq.' ') goto 2
c       enddo
c2      continue
c       do j=i,12
c        fname(j:j)=' '
c       enddo
       if (ntime.gt.1) call getname(fname,it)
       inquire(file=fname,exist=filflg)
       if (.not. filflg) then
        write(*,*) 'can not find file name ........ ',fname
        stop 0000
       endif
       open(2,file=fname,form='unformatted',status='old')
       read(2) ((u(i,j),i=1,np),j=1,ndof(n))
       close(2)
        row=arow(1)
        call readrow(c,m,row)
        write(3,*) (row(j:j),j=1,m)
        row=arow(2)
        call readrow(c,m,row)
        write(3,*) (row(j:j),j=1,m)
        write(3,*) 'Values'
        do i=1,np
         write(3,3000) i,(u(i,ipdofno(n,j)),j=1,npdof(n))
        enddo
        write(3,*) 'end Values'
        write(3,*)
1000  continue
1500  continue
2000  continue
      close(3)
 
 
c .... open msh file
      open(3,file='dzh.flavia.msh',form='formatted',status='unknown')
 
c .... open coor file
      open(2,file='../fuzhou-point-source/coor0',
     *  form='unformatted',status='old')
      read(2) np,nd,((coor(i,j),j=1,nd),i=1,np)
      close(2)
 
c .... open elem file
      open(2,file='../fuzhou-point-source/elem0',
     *  form='unformatted',status='old')
 
      n = 0
3     continue
      n = n+1
      read(1,'(a60)') row
      call readrow(c,m,row)
cc       write(*,*) (row(j:j),j=1,m)
      if (row(1:1).ne.'#') then
       write(3,*) (row(j:j),j=1,m)
       write(3,*) 'Coordinates'
       if (n.eq.1) then
        if (nd.eq.1) then
         z0=0.0d0
         do i=1,np
          write(3,3000) i,(coor(i,j),j=1,nd),z0
         enddo
         else
         do i=1,np
          write(3,3000) i,(coor(i,j),j=1,nd)
         enddo
        endif
       endif
       write(3,*) 'End coordinates'
       read(2) ne,nnode,(node(i),i=1,ne*nnode)
       write(3,*) 'Elements'
       do i=1,ne
        write(3,3100) i,(node((i-1)*nnode+j),j=1,nnode)
       enddo
       write(3,*) 'End elements'
       if (mate.eq.'y' .or. mate.eq.'Y') read(2) ne
       goto 3
      endif
      close(2)
      close(3)
      return
      end
 
      subroutine readrow(c,m,row)
       character row*60,c*3
cc       read(1,'(a60)') row
       do m=60,1,-1
        if (row(m:m).ne.' ') goto 1
       enddo
1      continue
       j=0
       if (m.gt.4) then
       do i=1,m
        if (row(i:i).eq.'#') then
         j=j+1
         row(i:i)=c(j:j)
        endif
       enddo
       endif
cc       write(*,*) (row(j:j),j=1,m)
      end
 
      subroutine getname(name,IT)
      implicit real*8 (a-h,o-z)
      character name*12,ch3*3
      call getext(it,ch3)
cc      write(*,*) 'name =',name,'++++ CH3 =',CH3
      do 10 i=1,12
      if (name(i:i).eq.' ') then
      j=i
      goto 20
      endif
10    continue
20    continue
      if (j.gt.9) then
      write(*,*) 'Error, plot filename too long .......',name
      write(*,*) ' the length of name must be less or equal 8 character'
      stop 111
      endif
cc     read(*,'(a3)') ch3
      name(j:j)='.'
      name(j+1:j+4)=ch3
cc     write(*,*) 'plot filename = ',name
      return
      end
 
      subroutine getext(ii,ch3)
      implicit real*8 (a-h,o-z)
      character ch3*3
      it = ii
      ch3 = '   '
      k = 0
      if (ii.ge.100) then
      n = it/100
      k = k+1
      call getchar(n,k,ch3)
      it = it - n*100
      endif
      if (ii.ge.10) then
      n = it/10
      k = k+1
      call getchar(n,k,ch3)
      it = it - n*10
      endif
      n = it
      k = k+1
      call getchar(n,k,ch3)
      return
      end
 
      subroutine getchar(n,k,ch3)
      implicit real*8 (a-h,o-z)
      character ch3*3
      if (n.eq.0) ch3(k:k) = '0'
      if (n.eq.1) ch3(k:k) = '1'
      if (n.eq.2) ch3(k:k) = '2'
      if (n.eq.3) ch3(k:k) = '3'
      if (n.eq.4) ch3(k:k) = '4'
      if (n.eq.5) ch3(k:k) = '5'
      if (n.eq.6) ch3(k:k) = '6'
      if (n.eq.7) ch3(k:k) = '7'
      if (n.eq.8) ch3(k:k) = '8'
      if (n.eq.9) ch3(k:k) = '9'
      return
      end
