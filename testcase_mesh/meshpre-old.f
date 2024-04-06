      program meshpre
c      open(10,file='coor0',form='unformatted',status='unknown')
      call coor0
c      close(10)
      open(10,file='elem0',form='unformatted',status='unknown')
      call elem80
      call mat80
      call elem40
      call mat40
      close(10)
c      open(10,file='elemb0',form='unformatted',status='unknown')
c      call elem80
c      call mat80
c      close(10)
      open(10,file='id0',form='unformatted',status='unknown')
      call id0
      close(10)
c      open(10,file='idb0',form='unformatted',status='unknown')
c      call idb0
c      close(10)
      open(10,file='disp0',form='unformatted',status='unknown')
      call disp0
      close(10)
c      open(10,file='dispb0',form='unformatted',status='unknown')
c      call disp10
c      close(10)
      end
      
      
      subroutine coor0
      implicit real*8 (a-h,o-z)
      parameter (max=5000000)
      common/nxy/nx,ny,nz,nn,mn0,nlayers,ndiv(8)
      common/earthp/pi,er,xlat0,xlat1,ylon0,ylon1
      common /coor3d / x3d(max),y3d(max),z3d(max),
     + thick(max)
      common /coor / x(max),y(max)
      common /lines/ xr1(500),yr1(500),xr2(500),yr2(500),
     +               xr3(500),yr3(500),xr4(500),yr4(500),
     +               xr5(500),yr5(500),xr6(500),yr6(500),
     +               xr7(500),yr7(500),xr8(500),yr8(500),nht(8),
     +               xrall(8,500),yrall(8,500)
      integer n
      write(*,*) '==================================================='
      write(*,*) 'the domain is a plane'
      write(*,*) 'following are requested parameters:'
      write(*,*) 'nx: the no. of segmentation of x-direction'
      write(*,*) 'ny: the no. of segmentation of y-direction'
      write(*,*) 'xmax: the distance of x-direction'
      write(*,*) 'ymax: the distance of y-direction'
      write(*,*) 'you can adjustfy the parameters in prefile.dat'
      write(*,*) '==================================================='
c      nx = 199
c      ny = 199
c      nz = 30

      open(10,file='prefile.dat',form='formatted',status='old')
      read(10,*)
      read(10,*)
      read(10,*) nx,ny,nzl,xlat0,xlat1,ylon0,ylon1
      read(10,*)
      read(10,*) nl
      do i = 1,nl
      	read(10,*) ndiv(i)
      end do
      close(10)
      
      pi = 3.14159265
      er = 6371.d0
      
c      
c      
      nht(1)=0
      nht(2)=0
      nht(3)=0
      nht(4)=0
      nht(5)=0
      nht(6)=0
      nht(7)=0
      nht(8)=0
c
c      ndiv(1) = 4
c      ndiv(2) = 2
c      ndiv(3) = 3
c      ndiv(4) = 4
c      ndiv(5) = 5
c      ndiv(6) = 8
c      ndiv(7) = 2
c      ndiv(8) = 1
c            
c      
c
c
c      
      open(30,file='vel_sur.dat',form='formatted',status='unknown')
      do i=1,500
      read(30,*,end=110) xr1(i),yr1(i) 
      nht(1) = nht(1) + 1
      end do
110   close(30)
c
      do i = 1,nht(1)
c      print *,i,xr1(i),yr1(i)
      end do
c      stop
c      
      m=nx+1
      n0=1
      n1=nht(1)
      call splin2(m,n0,n1,xr1,yr1)
c
      do i = 1,m
c      print *,i,xr1(i),yr1(i)
      end do
c
c      
      open(30,file='vel_g_1.dat',form='formatted',status='unknown')
      do i=1,500
      read(30,*,end=111) xr2(i),yr2(i) 
      nht(2) = nht(2) + 1
      end do
111   close(30)
c
c      do i = 1,nht(2)
c      print *,i,xr2(i),yr2(i)
c      end do
c      
      m=nx+1
      n0=1
      n1=nht(2)
      call splin2(m,n0,n1,xr2,yr2)
c
c      do i = 1,m
c      print *,i,xr2(i),yr2(i)
c      end do
c

c
      open(30,file='vel_c_1.dat',form='formatted',status='unknown')
      do i=1,500
      read(30,*,end=112) xr3(i),yr3(i) 
      nht(3) = nht(3) + 1
      end do
112   close(30)
c
c      do i = 1,nht(3)
c      print *,i,xr3(i),yr3(i)
c      end do
c      
      m=nx+1
      n0=1
      n1=nht(3)
      call splin2(m,n0,n1,xr3,yr3)
c
c      do i = 1,m
c      print *,i,xr3(i),yr3(i)
c      end do
c

c
      open(30,file='vel_c_2.dat',form='formatted',status='unknown')
      do i=1,500
      read(30,*,end=113) xr4(i),yr4(i) 
      nht(4) = nht(4) + 1
      end do
113   close(30)
c
c      do i = 1,nht(4)
c      print *,i,xr4(i),yr4(i)
c      end do
c      

      m=nx+1
      n0=1
      n1=nht(4)
      call splin2(m,n0,n1,xr4,yr4)
c
c      do i = 1,m
c      print *,i,xr4(i),yr4(i)
c      end do
c

c
      open(30,file='vel_c_3.dat',form='formatted',status='unknown')
      do i=1,500
      read(30,*,end=114) xr5(i),yr5(i) 
      nht(5) = nht(5) + 1
      end do
114   close(30)
c
c      do i = 1,nht(5)
c      print *,i,xr5(i),yr5(i)
c      end do
c      
      m=nx+1
      n0=1
      n1=nht(5)
      call splin2(m,n0,n1,xr5,yr5)
c
c      do i = 1,m
c      print *,i,xr5(i),yr5(i)
c      end do
c

c
      open(30,file='vel_m_1.dat',form='formatted',status='unknown')
      do i=1,500
      read(30,*,end=115) xr6(i),yr6(i) 
      nht(6) = nht(6) + 1
      end do
115   close(30)
c
c      do i = 1,nht(6)
c      print *,i,xr6(i),yr6(i)
c      end do
c      
      m=nx+1
      n0=1
      n1=nht(6)
      call splin2(m,n0,n1,xr6,yr6)
c
c      do i = 1,m
c      print *,i,xr6(i),yr6(i)
c      end do
c
c
      m=nx+1
      do i = 1,m
      xr7(i) = xr1(i)
      yr7(i) = 50.d0
      end do
c      
c      print *,(i,xr1(i),xr2(i),i=1,m)
c
c      open(30,file='vel_g_1.dat',form='formatted',status='unknown')
c      do i=1,nht(7)
c      read(30,*) xr7(i),yr7(i)
c      end do
c      close(30)
c
      do i =1,nx+1
      xrall(1,i) =  xr7(nx+1-i+1)*1000.d0
      yrall(1,i) = -yr7(i)*1000.d0
      xrall(2,i) =  xr6(nx+1-i+1)*1000.d0
      yrall(2,i) = -yr6(i)*1000.d0
      xrall(3,i) =  xr5(nx+1-i+1)*1000.d0
      yrall(3,i) = -yr5(i)*1000.d0
      xrall(4,i) =  xr4(nx+1-i+1)*1000.d0
      yrall(4,i) = -yr4(i)*1000.d0
      xrall(5,i) =  xr3(nx+1-i+1)*1000.d0
      yrall(5,i) = -yr3(i)*1000.d0
      xrall(6,i) =  xr2(nx+1-i+1)*1000.d0
      yrall(6,i) = -yr2(i)*1000.d0
      xrall(7,i) =  xr1(nx+1-i+1)*1000.d0
      yrall(7,i) = -yr1(i)*1000.d0
      end do
c
      open(30,file='part0.flavia.msh',status='unknown',
     +        form='formatted')
        write(30,*)'Mesh "W" Dimension 2 Elemtype Quadrilateral Nnode 4'
        write(30,*) 'Coordinates'
        nnn = 0
        do j = 1,7
        do i=1,nx+1
        nnn = nnn+1
        write(30,1000) nnn,xrall(j,i),yrall(j,i)
        end do
        end do
        write(30,*) 'End coordinates'
        write(30,*) 'Elements'
        nnn = 0
        do j =1,6
        do i = 1,nx
        nnn = nnn + 1
        write(30,1100) nnn,(nx+1)*(j-1)+i,(nx+1)*(j-1)+i+1,
     *  (nx+1)*j+i+1,(nx+1)*j+i,1
        end do
        end do
        write(30,*)'End elements'
        close(30)

      open(30,file='part0.flavia.res',status='unknown',
     +        form='formatted')
        write(30,*) 'GID Post Results File 1.0'
        write(30,*) 
        write(30,*) 'Result "ndispe" "Load Analysis" 1 Vector OnNodes'
        write(30,*) ' ComponentNames "u1" "u2"'
        write(30,*) 'Values'
        nnn = 0
        do j = 1,7
        do i=1,nx+1
        nnn = nnn+1
        write(30,1000) nnn,xrall(j,i),yrall(j,i)
        end do
        end do
        write(30,*) 'End values'
        close(30)
1000  format(i10,3e15.5)
1100  format(15i10)
1200  format(i10,e15.5)
1201   format(i10,10e15.5)         
c
c
      n=0
      nlayers = 6
      do  3 k=1,nlayers
      do  2 j=1,ndiv(k)
      do  1 i=1,nx+1
      n=n+1
      if (n.le.0) goto  1
      x(n)=xrall(k,i)+(xrall(k+1,i)-xrall(k,i))/ndiv(k)*(j-1)
      y(n)=yrall(k,i)+(yrall(k+1,i)-yrall(k,i))/ndiv(k)*(j-1)
   1  continue
   2  continue
   3  continue
      do  i=1,nx+1
      n=n+1
      x(n)=xrall(7,i)
      y(n)=yrall(7,i)
      end do
      nn=n
      nyl = 0
      do i = 1,nlayers
      nyl = nyl+ndiv(i)
      end do
c      
      open(30,file='part1.flavia.msh',status='unknown',
     +        form='formatted')
        write(30,*) 'Mesh "Whole" Dimension 2',
     +  ' Elemtype Quadrilateral Nnode 4'
        write(30,*) 'Coordinates'
        do i=1,nn
        write(30,1000) i,x(i),y(i)
        end do
        write(30,*) 'End coordinates'
        write(30,*) 'Elements'
        nnn = 0
        do j=1,nyl
        do i = 1,nx
        nnn = nnn + 1
      write(30,1100) nnn,(nx+1)*(j-1)+i,(nx+1)*(j-1)+i+1,
     *  (nx+1)*j+i+1,(nx+1)*j+i,1
        end do
        end do
        write(30,*)'End elements'
        close(30)
c
c
c
c     read 3-dimension file and data
c        
c
c      
c      nx = 199
c      ny = 199
c      open(30,file='fuzhouh_200.dat',form='formatted',status='unknown')
c      n = 0
c      do i = 1,200
c      read(30,*)(z3d((i-1)*200+j),j=1,200)
c      end do
c      close(30)

      do j = 1,ny+1
      do i = 1,nx+1
      	z3d((i-1)*(nx+1)+j) = 100.
      end do
      end do
      
      do j = 1,ny+1
      do i = 1,nx+1
      	thick((i-1)*(nx+1)+j) = 20.
      end do
      end do


      open(30,file='surfacetopodata',form='formatted',
     +     status='unknown')
      n = 0
      do j = 1,200
        do i = 1,200
      read(30,*)thick((j-1)*200+i)
      end do
      end do
      close(30)
c
c
c      do i = 1,200
c      do j = 1,200
c      if(z3d((i-1)*200+j).gt.100.d0) thick((i-1)*200+j) = 20.d0
c      end do
c      end do
c
c
c
      xexpand = (xlat1-xlat0)*pi*er*1000.d0/180.d0
      yexpand = (ylon1-ylon0)*pi*er*1000.d0/180.d0
      do k = 1,ndiv(1)+1
      do i = 1,(ny+1)
      do j = 1,(nx+1)
      n = (k-1)*(nx+1)*(ny+1)+(i-1)*(nx+1)+j
      x3d(n)= xexpand/nx*(j-1)
      y3d(n)= yexpand/ny*(i-1)
      z3d(n) =z3d((i-1)*nx+j)-thick((i-1)*nx+j)/ndiv(1)*(k-1)
      end do
      end do
      end do      
c
c
c     
        open(30,file='part2.flavia.msh',status='unknown',
     +        form='formatted')
        write(30,*)'Mesh "W" Dimension 3 Elemtype Hexahedra Nnode 8'
        write(30,*) 'Coordinates'
        do i=1,(nx+1)*(ny+1)
        write(30,1000) i,x3d(i),y3d(i),z3d(i)
        end do
        do i=1,(nx+1)*(ny+1)
        write(30,1000) i+(nx+1)*(ny+1),x3d(i),
     +                 y3d(i),z3d(i)+thick(i)
        end do
        write(30,*) 'End coordinates'
        write(30,*) 'Elements'
        n = 0
        nadd = (nx+1)*(ny+1)
        do i = 1,ny
        do j = 1,nx
        n = n + 1
        write(30,1100) n,(nx+1)*(i-1)+j,(nx+1)*(i-1)+j+1,
     +                   (nx+1)*i+j+1,(nx+1)*i+j,
     +                   nadd+(nx+1)*(i-1)+j,nadd+(nx+1)*(i-1)+j+1,
     +                   nadd+(nx+1)*i+j+1,nadd+(nx+1)*i+j,1   
        end do
        end do
        write(30,*)'End elements'
        close(30)

        open(30,file='part2.flavia.res',status='unknown',
     +        form='formatted')
        write(30,*) 'GID Post Results File 1.0'
        write(30,*) 
        write(30,*)'Result "thick" "Load Analysis" 1 Scalar OnNodes'      
        write(30,*) ' ComponentNames "thick"'
        write(30,*) 'Values'
        do i=1,(nx+1)*(ny+1)
        write(30,1000) i,thick(i)
        end do
        do i=1,(nx+1)*(ny+1)
        write(30,1000) i+(nx+1)*(ny+1),thick(i)
        end do
        write(30,*) 'End values'
        write(30,*) 
        write(30,*)'Result "evel" "Load Analysis" 1 Scalar OnNodes'      
        write(30,*) ' ComponentNames "evel"'
        write(30,*) 'Values'
        do i=1,(nx+1)*(ny+1)
        write(30,1000) i,z3d(i)
        end do
        do i=1,(nx+1)*(ny+1)
        write(30,1000) i+(nx+1)*(ny+1),z3d(i)
        end do
        write(30,*) 'End values'
        close(30)

        open(30,file='part3.flavia.msh',status='unknown',
     +        form='formatted')
        write(30,*)'Mesh "W" Dimension 2 Elemtype Quadrilateral Nnode 4'
        write(30,*) 'Coordinates'
        do i=1,(nx+1)*(ny+1)
        write(30,1000) i,x3d(i),y3d(i)
        end do
        write(30,*) 'End coordinates'
        write(30,*) 'Elements'
        n = 0
        nadd = (nx+1)*(ny+1)
        do i = 1,ny
        do j = 1,nx
        n = n + 1
        write(30,1100) n,(nx+1)*(i-1)+j,(nx+1)*(i-1)+j+1,
     +                (nx+1)*i+j+1,(nx+1)*i+j,1   
        end do
        end do
        write(30,*)'End elements'
        close(30)

        open(30,file='part3.flavia.res',status='unknown',
     +        form='formatted')
        write(30,*) 'GID Post Results File 1.0'
        write(30,*) 
        write(30,*) 'Result "ndispe" "Load Analysis" 1 Vector OnNodes'
        write(30,*) ' ComponentNames "u1","u2"'
        write(30,*) 'Values'
        do i=1,(nx+1)*(ny+1)
        write(30,1000) i,thick(i),thick(i)
        end do
        write(30,*) 'End values'
        close(30)
c
c       expand to 3d mesh for seisimology computing
c
c
c      xexpand = (99.-92.)*3.14159265*6371.d0*1000.d0/180.d0
c      yexpand = (31.-27.)*3.14159265*6371.d0*1000.d0/180.d0
c      print *,xexpand,yexpand
c            
      n = 0
      nlall = 0
      nlayers = 6
      do j = 1,ny+1
      do i = 1,nx+1
      n = n+1
      x(n) = z3d(n)
      end do
      end do
c
      n = 0
      nlall = 0
      nlayers = 7
c
      do  nl=1,nlayers-1
      do  k=1,ndiv(nl)
      nlall = nlall + 1
      do  j=1,ny+1
      do  i=1,nx+1
      n0 = (j-1)*(nx+1)+i
      n = (nlall-1)*(nx+1)*(ny+1)+(j-1)*(nx+1)+i
      x3d(n)= xexpand/nx*(i-1)
      y3d(n)= yexpand/ny*(j-1)
      if(nl.eq.1) then
      z3d(n)= yrall(nl,i)+(yrall(nl+1,i)-yrall(nl,i))/ndiv(nl)*(k-1)
      else
c      z3d(n)= x(n0)
c     +       +yrall(nl,i)+(yrall(nl+1,i)-yrall(nl,i))/ndiv(nl)*(k-1)
      z3d(n)= x(n0)
     +       +yrall(nl,i)+(yrall(nl+1,i)-yrall(nl,i))/ndiv(nl)*(k-1)
      end if 
      end do
      end do
      end do
      end do
c      
c
c     enclosure the whole layers
c
c      do k = 1,1
c      do j = 1,ny+1
c      do i = 1,nx+1
c      n = n+1
c      x3d(n)= xexpand/199*(i-1)
c      y3d(n)= yexpand/199*(j-1)
c      z3d(n)= x(n0)+yrall(nlayers,i)
c      end do
c      end do
c      end do            
c
c     add 4th futu layer thickness
c
      do k = 1,ndiv(nlayers)+1
      do j = 1,ny+1
      do i = 1,nx+1
      n = n+1
      x3d(n)= xexpand/nx*(i-1)
      y3d(n)= yexpand/ny*(j-1)
      z3d(n)=z3d(n-(nx+1)*(ny+1))
     *   +thick((i-1)*(nx+1)+j)/ndiv(nlayers)*(k-1)
      end do
      end do
      end do      
c
c
c
      nn=n
      nz = 0
      do i = 1,nlayers
      nz = nz+ndiv(i)
      end do
c
c       write out coordinate data
c        
c
c      print *,nx,ny,nz,z3d(1),z3d(nn),z3d(nn)-z3d(1)
      mn0 = 0
      mtij=(nx+1)*(ny+1)*(nz+1)
      mtik=  3
      print*, mtij,mtik,n
      open(10,file='coor0',form='unformatted',status='unknown')
      write(10) mtij,mtik,
     * (x3d(mtii),y3d(mtii),z3d(mtii),mtii=1,n)
      print*, mtij,mtik,mtii
      close(10)
     
      open(30,file='nodes.dat',form='formatted',status='unknown')
      write(30,1100) (nx+1)*(ny+1)
      do j = 1,(ny+1)
      	do i = 1,(nx+1)
      		n = (j-1)*(nx+1)+i
      		write(30,1000) n,x3d(n),y3d(n)
      	end do
      end do
      close(30)
      open(30,file='layerinfo',form='formatted',status='unknown')
      write(30,*) 'Nlayer,nx,ny'
      write(30,1100) nzl-2,nx,ny
      close(30)
      
      return
      end
 
      subroutine elem80
      implicit real*8 (a-h,o-z)
      parameter (max=5000000)
      common/nxy/nx,ny,nz,nn,mn0,nlayers,ndiv(8)
      common/earthp/pi,er,xlat0,xlat1,ylon0,ylon1
      common /coor3d / x3d(max),y3d(max),z3d(max),
     + thick(max)      
      common /coor / x(max),y(max)
      common /elem8 / nod1(max),nod2(max),nod3(max),
     *nod4(max),nod5(max),nod6(max),nod7(max),
     *nod8(max),m8(max)
      integer n,nod1,nod2,nod3,nod4,nod5,nod6,nod7,nod8,m8
      do i = 2,nlayers
      ndiv(i) = ndiv(i-1)+ndiv(i)
      end do
c      print *,nx,ny,nz
      do  6 k=1,nz
      do  5 j=1,ny
      do  4 i=1,nx
      n=(k-1)*nx*ny+(j-1)*(nx)+i
      if (n.le.0) goto  4
      nod1(n)=(k-1)*(nx+1)*(ny+1)+(j-1)*(nx+1)+i
      nod2(n)=(k-1)*(nx+1)*(ny+1)+(j-1)*(nx+1)+i+1
      nod3(n)=(k-1)*(nx+1)*(ny+1)+j*(nx+1)+i+1
      nod4(n)=(k-1)*(nx+1)*(ny+1)+j*(nx+1)+i
      nod5(n)=(k)*(nx+1)*(ny+1)+(j-1)*(nx+1)+i
      nod6(n)=(k)*(nx+1)*(ny+1)+(j-1)*(nx+1)+i+1
      nod7(n)=(k)*(nx+1)*(ny+1)+j*(nx+1)+i+1
      nod8(n)=(k)*(nx+1)*(ny+1)+j*(nx+1)+i
      do l = 1,nlayers-1
      if(k.le.ndiv(1)) m8(n) = 1
      if((k.gt.ndiv(l)).and.(k.le.ndiv(l+1))) m8(n) = l+1
c      if(k.gt.ndiv(nlayers)) m8(n) = nlayers+1
      end do
c
c      
      nabsorb = 3
      if((i.lt.nabsorb).or.(i.gt.(nx-nabsorb)+1)) m8(n) = nlayers+1
      if((j.lt.nabsorb).or.(j.gt.(ny-nabsorb)+1)) m8(n) = nlayers+1
      if(k.le.nabsorb-2) m8(n) = nlayers+1
c
   4  continue
   5  continue
   6  continue
      mtij=n
      mtik=  9
      write(10) mtij,mtik,
     *(nod1(mtii),nod2(mtii),nod3(mtii),nod4(mtii),
     * nod5(mtii),nod6(mtii),nod7(mtii),nod8(mtii),m8(mtii),mtii=1,n)
      
      open(30,file='trigs.dat',form='formatted',status='unknown')
      write(30,1100) nx*ny,4
      do j = 1,ny
      	do i = 1,nx
      		n = (j-1)*nx+i
      		write(30,1100) n,nod1(n),nod2(n),nod3(n),nod4(n)
      	end do
      end do
      close(30)
      
      
c
c      goto 9999
      if(mn0.eq.0) then
      open(30,file='partition.flavia.msh',status='unknown',
     +        form='formatted')
        write(30,*) 'Mesh "Whole" Dimension 3',
     +  ' Elemtype Hexahedra Nnode 8'
        write(30,*) 'Coordinates'
        do i=1,nn
        write(30,1000) i,x3d(i),y3d(i),z3d(i)
        end do
        write(30,*) 'End coordinates'
        write(30,*) 'Elements'
        do mtii=1,mtij
      write(30,1100) mtii,nod1(mtii),nod2(mtii),nod3(mtii),nod4(mtii),
     *nod5(mtii),nod6(mtii),nod7(mtii),nod8(mtii),m8(mtii)
        end do
        write(30,*)'End elements'
        close(30)
        mn0 = 1
        end if
9999    continue
1000  format(i10,3e15.5)
1100  format(15i10)
1200  format(i10,e15.5)
1201   format(i10,10e15.5)        
c
c
c     
      return
      end
 
      subroutine mat80
      implicit real*8 (a-h,o-z)
      parameter (max=5000000)
      common/nxy/nx,ny,nz,nn,mn0,nlayers,ndiv(8)
      common/earthp/pi,er,xlat0,xlat1,ylon0,ylon1
      common /mat8 /pe(9),pv(9),fx(9),fy(9),fz(9),rou(9),alfa(9)
      common /materials/density(10),vp(10),vs(10),qc(10)
      dimension alamda(10),amu(10)
      integer n
      open(30,file='materials',form='formatted',status='old')
      read(30,*)
      read(30,*)
      read(30,*)
      read(30,*)
      read(30,*)
      read(30,*)
      read(30,*)
      read(30,*) nlayers0
      read(30,*)
      do i = 1,nlayers0
      read(30,*) density(i),vp(i),vs(i),qc(i),fx(i),fz(i)
c      write(*,*) density(i),vp(i),vs(i),qc(i),fx(i),fz(i)
      end do
      close(30)
      n = 0
      do i = 1,nlayers0
      vs(i) = dsqrt(3.d0)/3.d0*vp(i)
      density(i) = vp(i)/3.d0+1280
      if(qc(i).lt.1.0) qc(i) = 1.d0
      qc(i) = 1.d0/(2.d0*pi*qc(i))
      if(density(i).lt.2000.d0) density(i)=2000.d0
      amu(i)=density(i)*vs(i)*vs(i)
      alamda(i)=density(i)*(vp(i)*vp(i)-2.d0*vs(i)*vs(i))
      if (amu(i).lt.0.d0) then
      print *, 'Warning: minus coefficients' 
      end if
      if (alamda(i).lt.0.d0) then
      print *, 'Warning: minus coefficients' 
      end if
      n=i
      pe(n)=alamda(i)
      pv(n)=amu(i)
      fx(n)=fx(i)
      fy(n)=0.d0
      fz(n)=fz(i)
      rou(n) = density(i)
      alfa(n) = qc(i)
      end do
      mtij=n
      mtik=  7
      write(10) mtij,mtik,
     *(pe(mtii),pv(mtii),fx(mtii),fy(mtii),fz(mtii),
     *rou(mtii),alfa(mtii),mtii=1,n)
      return
      end
 
      subroutine elem40
      implicit real*8 (a-h,o-z)
      parameter (max=5000000)
      common/nxy/nx,ny,nz,nn,mn0,nlayers,ndiv(8)
      common/earthp/pi,er,xlat0,xlat1,ylon0,ylon1
      common /elem4 / nod1(max),nod2(max),
     *nod3(max),nod4(max),m4(max)
      integer n,nod1,nod2,nod3,nod4,m4
c
c     lower face
c
      n  = 0
      do j = 1,ny
      do i = 1,nx
      n = n + 1
      nod1(n)=(j-1)*(nx+1)+i
      nod2(n)=(j-1)*(nx+1)+i+1
      nod3(n)=j*(nx+1)+i+1
      nod4(n)=j*(nx+1)+i
      m4(n)=1
      end do
      end do

c
c     upper face
c
      do j = 1,ny
      do i = 1,nx
      n = n + 1
      nod1(n)=(nx+1)*(ny+1)*nz+(j-1)*(nx+1)+i
      nod2(n)=(nx+1)*(ny+1)*nz+(j-1)*(nx+1)+i+1
      nod3(n)=(nx+1)*(ny+1)*nz+j*(nx+1)+i+1
      nod4(n)=(nx+1)*(ny+1)*nz+j*(nx+1)+i
      m4(n)=2
      end do
      end do
c
c     front face
c
      do j = 1,nz
      do i = 1,nx
      n = n + 1
      nod1(n)=(nx+1)*(ny+1)*(j-1)+i
      nod2(n)=(nx+1)*(ny+1)*(j-1)+i+1
      nod3(n)=(nx+1)*(ny+1)*(j)+i+1
      nod4(n)=(nx+1)*(ny+1)*(j)+i
      m4(n)=3
      end do
      end do
c
c     back face
c
      do j = 1,nz
      do i = 1,nx
      n = n + 1
      nod1(n)=(nx+1)*(ny+1)*(j-1)+ny*(nx+1)+i
      nod2(n)=(nx+1)*(ny+1)*(j-1)+ny*(nx+1)+i+1
      nod3(n)=(nx+1)*(ny+1)*(j)+ny*(nx+1)+i+1
      nod4(n)=(nx+1)*(ny+1)*(j)+ny*(nx+1)+i
      m4(n)=4
      end do
      end do
c
c     left face
c
      do j = 1,nz
      do i = 1,ny
      n = n + 1
      nod1(n)=(nx+1)*(ny+1)*(j-1)+(i-1)*(nx+1)+1
      nod2(n)=(nx+1)*(ny+1)*(j-1)+i*(nx+1)+1
      nod3(n)=(nx+1)*(ny+1)*(j)+i*(nx+1)+1
      nod4(n)=(nx+1)*(ny+1)*(j)+(i-1)*(nx+1)+1
      m4(n)=5
      end do
      end do
c
c     right face
c
      do j = 1,nz
      do i = 1,ny
      n = n + 1
      nod1(n)=(nx+1)*(ny+1)*(j-1)+(i-1)*(nx+1)+(nx+1)
      nod2(n)=(nx+1)*(ny+1)*(j-1)+i*(nx+1)+(nx+1)
      nod3(n)=(nx+1)*(ny+1)*(j)+i*(nx+1)+(nx+1)
      nod4(n)=(nx+1)*(ny+1)*(j)+(i-1)*(nx+1)+(nx+1)
      m4(n)=6
      end do
      end do

      mtij=1
      mtik=  5
      write(10) mtij,mtik,
     *(nod1(mtii),nod2(mtii),nod3(mtii),nod4(mtii),m4(mtii),mtii=1,mtij)
      return
      end
 
      subroutine mat40
      implicit real*8 (a-h,o-z)
      parameter (max=5000000)
      common/nxy/nx,ny,nz,nn,mn0,nlayers,ndiv(8)
      common/earthp/pi,er,xlat0,xlat1,ylon0,ylon1
      common /mat4 / fx(100),fy(100),fz(100)
      integer n
      n=1
      fx(n)=0.d00
      fy(n)=0.d00
      fz(n)=0.d00
      n=2
      fx(n)=0.d00
      fy(n)=0.d00
      fz(n)=0.d00
      mtij=n
      mtik=  3
      write(10) mtij,mtik,
     *(fx(mtii),fy(mtii),fz(mtii),mtii=1,n)
      return
      end
 
      subroutine id0
      implicit real*8 (a-h,o-z)
      parameter (max=5000000)
      common/nxy/nx,ny,nz,nn,mn0,nlayers,ndiv(8)
      common/earthp/pi,er,xlat0,xlat1,ylon0,ylon1
      common /id / idu(max),idv(max),idw(max)
      integer n,idu,idv,idw
      do 10 i=1,nn
      n=i
      if (n.le.0) goto 10
      idu(n)=1
      idv(n)=1
      idw(n)=1
  10  continue
c
c     lower face
c
      do j = 1,ny+1
      do i = 1,nx+1
      n = (j-1)*(nx+1)+i
      idu(n) = -1
      idv(n) = -1
      idw(n) = -1
      end do
      end do
c
c     upper face
c
      do j = 1,ny+1
      do i = 1,nx+1
      n = (nx+1)*(ny+1)*nz+(j-1)*(nx+1)+i
      idu(n) = 1
      idv(n) = 1
      idw(n) = 1
      end do
      end do
c
c     front face
c
      do j = 1,nz+1
      do i = 1,nx+1
      n = (nx+1)*(ny+1)*(j-1)+i
      idu(n) = -1
      idv(n) = -1
      idw(n) = -1
      end do
      end do
c
c     back face
c
      do j = 1,nz
      do i = 1,nx
      idu(n) = -1
      idv(n) = -1
      idw(n) = -1
      n = (nx+1)*(ny+1)*(j-1)+ny*(nx+1)+i
      end do
      end do
c
c     left face
c
      do j = 1,nz
      do i = 1,ny
      n = (nx+1)*(ny+1)*(j-1)+(i-1)*(nx+1)+1
      idu(n) = -1
      idv(n) = -1
      idw(n) = -1
      end do
      end do
c
c     right face
c
      do j = 1,nz
      do i = 1,ny
      n = (nx+1)*(ny+1)*(j-1)+(i-1)*(nx+1)+(nx+1)
      idu(n) = -1
      idv(n) = -1
      idw(n) = -1
      end do
      end do
c
c     seismological source 
c
c
      imode = 2
      if(imode.eq.1) then
      
      open(31,file='constrain',status='unknown',form='formatted')
      write(31,*) 70
      laysource = 18
      do j = 100,100
      do i = 80,150
      n = (nx+1)*(ny+1)*laysource+(j-1)*(nx+1)+i
      idu(n) = -1
      idv(n) = -1
      idw(n) = -1
      write(31,*) n
      end do
      end do
      close(31)
      
      else if(imode.eq.2) then
      
      open(31,file='constrain',status='unknown',form='formatted')
      write(31,*) 1
      laysource = 18
      do j = (ny+1)/2,(ny+1)/2
      do i = (nx+1)/2,(nx+1)/2
      n = (nx+1)*(ny+1)*laysource+(j-1)*(nx+1)+i
      idu(n) = -1
      idv(n) = -1
      idw(n) = -1
      write(31,*) n
      end do
      end do
      close(31)
      
      else
      write(*,*) 'Please select the proper mode in fuzhoupre'
      end if 
c
      open(30,file='partition.flavia.res',status='unknown',
     +        form='formatted')
        write(30,*) 'GID Post Results File 1.0'
        write(30,*) 
        write(30,*) 'Result "ndispe" "Load Analysis" 1 Vector OnNodes'
        write(30,*) ' ComponentNames "u1" "u2" "u3"'
        write(30,*) 'Values'
        do i=1,(nx+1)*(ny+1)*(nz+1)
        xid=idu(i)
        yid=idv(i)
        zid=idw(i)
        write(30,1000) i,xid,yid,zid
        end do
        write(30,*) 'End values'
        close(30)
1000  format(i10,3e15.5)
1100  format(15i10)
1200  format(i10,e15.5)
1201   format(i10,10e15.5)         
c
c      
c  
      mtij = nn
      mtik = 3
      write(10) mtij,mtik,
     *(idu(mtii),idv(mtii),idw(mtii),mtii=1,mtij)
      return
      end
 
      subroutine disp0
      implicit real*8 (a-h,o-z)
      parameter (max=5000000)
      common/nxy/nx,ny,nz,nn,mn0,nlayers,ndiv(8)
      common/earthp/pi,er,xlat0,xlat1,ylon0,ylon1
      common /disp / u(max),v(max),w(max)
      integer n
      do 13 i=1,nn
      n=i
      if (n.le.0) goto 13
      u(n)=0.00
      v(n)=0.00
      w(n)=0.00
  13  continue
c
c     lower face
c
      do j = 1,ny+1
      do i = 1,nx+1
      n = (j-1)*(nx+1)+i
      u(n)=0.d0
      v(n)=0.d0
      w(n)=0.d0
      end do
      end do
c
c     upper face
c
      do j = 1,ny+1
      do i = 1,nx+1
      n = (nx+1)*(ny+1)*nz+(j-1)*(nx+1)+i
      u(n)=0.d0
      v(n)=0.d0
      w(n)=0.d0
      end do
      end do
c
c     front face
c
      do j = 1,nz+1
      do i = 1,nx+1
      n = (nx+1)*(ny+1)*(j-1)+i
      u(n)=0.d0
      v(n)=0.d0
      w(n)=0.d0
      end do
      end do
c
c     back face
c
      do j = 1,nz
      do i = 1,nx
      u(n)=0.d0
      v(n)=0.d0
      w(n)=0.d0
      n = (nx+1)*(ny+1)*(j-1)+ny*(nx+1)+i
      end do
      end do
c
c     left face
c
      do j = 1,nz
      do i = 1,ny
      n = (nx+1)*(ny+1)*(j-1)+(i-1)*(nx+1)+1
      u(n)=0.d0
      v(n)=0.d0
      w(n)=0.d0
      end do
      end do
c
c     right face
c
      do j = 1,nz
      do i = 1,ny
      n = (nx+1)*(ny+1)*(j-1)+(i-1)*(nx+1)+(nx+1)
      u(n)=0.d0
      v(n)=0.d0
      w(n)=0.d0
      end do
      end do
c
      mtij = nn
      mtik = 3
      write(10) mtij,mtik,(u(mtii),v(mtii),w(mtii),mtii=1,mtij)
      return
      end

      subroutine splin2(m,n0,n1,x,y)
      implicit REAL*8 (a-h,o-z)
      dimension tl(400),xl(400),yl(400),zl(400),tt(1000),
     & x(400),y(400),z(400)
     
      do 500 k=n0,n1
      nl = k-n0+1
      xl(nl) = x(k)
      yl(nl) = y(k)
      zl(nl) = 0.d0
500   continue
c     WRITE(*,*) 'm,nl =',m,nl,'  xl,yl ='
c     WRITE(*,*) (xl(i),yl(i),i=1,nl)
cc        WRITE(*,*) 'm,nl =',m,nl,'  xl,yl ='
      call gets(nl,xl,yl,zl,tl)
cc        WRITE(*,*) (xl(i),i=1,nl)
cc        WRITE(*,*) (yl(i),i=1,nl)
cc       write(*,*) 'NL =',NL
      if (nl.le.2) then
      dx = (xl(2)-xl(1))/(m-1)
      dy = (yl(2)-yl(1))/(m-1)
      dz = (zl(2)-zl(1))/(m-1)
      do 600 k=1,m-1
      x(k+n0) = x(n0)+dx*k
      y(k+n0) = y(n0)+dy*k
      z(k+n0) = z(n0)+dy*k
600   continue
      else
      dt=tl(nl)/(m-1)
      tt(1)=0.0
      do 800 i=2,m
      tt(i)=tt(i-1)+dt
800   continue
cc        write(*,*) 'tt =',(tt(i),i=1,m)
      call splin1(nl,m,tl,xl,tt,x(n0))
      call splin1(nl,m,tl,yl,tt,y(n0))
      call splin1(nl,m,tl,zl,tt,z(n0))
      endif
      N1 = N0+M-1
c      write(*,*) 'NL,N0,N1 = ',NL,N0,N1,'  x & y ='
c      write(*,*) (x(i),i=n0,n1)
c      write(*,*) (y(i),i=n0,n1)
      return
      end
 
      subroutine splin1(nl,m,xl,yl,xx,yy)
      implicit REAL*8 (a-h,o-z)
      dimension xl(*),yl(*),dy(200),
     *            h(200),xx(*),yy(*)
c        double precision x,y,dy,ddy,h,h0,h1,a,b
c     WRITE(*,*) 'nl,m =',nl,m,'  xl,yl ='
c     WRITE(*,*) (xl(i),i=1,nl)
c     WRITE(*,*) (yl(i),i=1,nl)
c     WRITE(*,*) 'xx =',(xx(i),i=1,m)
      dy(1)=-0.5
      h0=xl(2)-xl(1)
      h(1)=3.0*(yl(2)-yl(1))/(2.0*h0)
      do 10 j=2,nl-1
      h1=xl(j+1)-xl(j)
      a=h0/(h0+h1)
      b=(1.0-a)*(yl(j)-yl(j-1))/h0
      b=3.0*(b+a*(yl(j+1)-yl(j))/h1)
      dy(j)=-a/(2.0+(1.0-a)*dy(j-1))
      h(j)=(b-(1.0-a)*h(j-1))
      h(j)=h(j)/(2.0+(1.0-a)*dy(j-1))
      h0=h1
10    continue
      dy(nl)=(3.0*(yl(nl)-yl(nl-1))/h1-h(nl-1))/(2.0+dy(nl-1))
        do 20 j=nl-1,1,-1
      dy(j)=dy(j)*dy(j+1)+h(j)
20    continue
      do 30 j=1,nl-1
      h(j)=xl(j+1)-xl(j)
30    continue
      do 70 j=1,m
       if (xx(j).ge.xl(nl)) then
         i=nl-1
       else
         i=1
60       if (xx(j).gt.xl(i+1)) then
            i=i+1
          goto 60
         endif
       endif
      h1=(xl(i+1)-xx(j))/h(i)
      yy(j)=(3.0*h1*h1-2.0*h1*h1*h1)*yl(i)
      yy(j)=yy(j)+h(i)*(h1*h1-h1*h1*h1)*dy(i)
      h1=(xx(j)-xl(i))/h(i)
      yy(j)=yy(j)+(3.0*h1*h1-2.0*h1*h1*h1)*yl(i+1)
      yy(j)=yy(j)-h(i)*(h1*h1-h1*h1*h1)*dy(i+1)
70    continue
c     WRITE(*,*) 'yy =',(yy(i),i=1,m)
      return
      end
 
      subroutine gets(n,x,y,z,s)
      implicit REAL*8 (a-h,o-z)
      dimension s(*),x(*),y(*),z(*)
c     write(*,*) 'n = ',n
      s0=sqrt((x(n)-x(1))**2+(y(n)-y(1))**2+(z(n)-z(1))**2)*1.0d-6
      s(1)=0.0
      do 100 k=2,n
      s(k)=sqrt((x(k)-x(k-1))**2+(y(k)-y(k-1))**2+(z(k)-z(k-1))**2)
100   continue
      m = 1
      do 200 k=2,n
      if (s(k).gt.s0) then
      m = m+1
      s(m) = s(k)
      x(m) = x(k)
      y(m) = y(k)
      endif
200   continue
      do 300 k=2,m
      s(k) = s(k)+s(k-1)
300   continue
c     WRITE(*,*) 'm =',m,'  s = '
c     write(*,*) (s(i),i=1,m)
      n = m
      return
      end
 
