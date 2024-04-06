      program Mazpartition

      implicit double precision (a-h,o-z)
      character*20 fname,fname1,fname2
      character*20 filename(20)
      character*20 filename1(20)
      character*3 uname(20)
      character*60 chartemp
      logical filflgdisp(20)
      dimension disid(10)
       
      integer,allocatable::trigs(:,:)
                  
      double precision,allocatable::coor(:,:)

      integer,allocatable::idelem(:)
      integer,allocatable::idnode(:)
      
      integer,allocatable::ieptr(:)
      integer,allocatable::ieind(:)

c
c     Now, lets start the game ... ...
c
c
c     Get the element data file and domain partition number'
      filename(19) = 'elem0.elm'
c
c     Reading through all the triangle mesh data
c     
      open(30,file='nodes.dat',status='unknown',
     +        form='formatted')
      read(30,*) knode
      write(*,*) 'knode ===',knode

      allocate(coor(3,knode)) 

      do i = 1,knode
      read(30,*) itemp,coor(1,i),coor(2,i)
      end do
      close(30)

c
c
      open(30,file='trigs.dat',status='unknown',
     +        form='formatted')
      read(30,*) ntri
      write(*,*) 'num ===',ntri
      
      allocate(trigs(5,ntri)) 
      allocate(ieind(ntri*4))
      allocate(ieptr(ntri+1))
      
      
      do i=1,ntri
      read(30,*)it,(trigs(j,i),j=1,4)
      end do
      close(30)
c
      do i=1,ntri
      trigs(5,i) = 1
      end do
      
      k = 0
      ke = 1
      ieptr(ke) = 0
      do i = 1,ntri
      ke = ke+1
      do j = 1,4
      k = k+1
      ieind(k)=trigs(j,i)-1
      end do
      ieptr(ke) = k
      end do
c      print*,ke

C
C.......Start Data partition.....................................

      numpartition = ntri
      nne = 4
      ncoor = 2

      nelemtype=0
      if(nne.eq.3) nelemtype=1
      if((nne.eq.6).and.(ncoor.eq.3)) nelemtype=1
      if((nne.eq.4).and.(ncoor.eq.2)) nelemtype=4
      if((nne.eq.4).and.(ncoor.eq.3)) nelemtype=2
      if((nne.eq.8).and.(ncoor.eq.3)) nelemtype=3
      if((nne.eq.10).and.(ncoor.eq.3)) nelemtype=2
      if(nelemtype.eq.1) filename1(20) = 'Triangle'
      if(nelemtype.eq.2) filename1(20) = 'Tetrahedra'
      if(nelemtype.eq.3) filename1(20) = 'Hexahedra'
      if(nelemtype.eq.4) filename1(20) = 'Quadrilateral'
c
c     domain partition according to the first layer information
c
c
      open(2,file='part',form='formatted',status='unknown')
      read(2,*) numblk
      close(2)



c
c       get the partition data for each subdomain
c
      allocate(idelem(numpartition))
      allocate(idnode(knode))    

      do i = 1, numall
      idelem(i) = 0
      end do
      
      do i = 1, knode
      idnode(i) = 0
      end do
       
      
      ietype = 1
c
      print*, 'Subroutine begin'
      call partdmain(ntri,knode,ieptr,ieind,numblk,ietype,
     *  idelem,idnode)
c
c
      
      print*, 'Subroutine end'
      
      do i=1,ntri
      trigs(5,i) = idelem(i)
      end do

c
c...... Open Gid file for partition checking
c
        ncoor = 2
        
        open(10,file='partition.flavia.msh',status='unknown',
     +       form='formatted')
        write(10,1550) ' Mesh "Whole" Dimension',2,
     + ' Elemtype ','Quadrilateral',' Nnode',4
        write(10,*) 'Coordinates'
        do i=1,knode
        write(10,1000) i,(coor(j,i),j=1,2)
        end do
        write(10,*) 'End coordinates'
        write(10,*) 'Elements'
        do i=1,ntri
        write(10,1100) i,(trigs(j,i),j=1,5)
        end do
        write(10,*)'End elements'
        close(10)
        
        open(10,file='idnode',form='formatted',status='unknown')
        write(10,*) knode
        do i = 1,knode
        	write(10,*) idnode(i)
        end do
        close(10)
        
        open(10,file='idelem',form='formatted',status='unknown')
        write(10,*) ntri
        do i = 1,ntri
        	write(10,*) idelem(i)
        end do
        close(10)
        
        
      deallocate(coor)

      deallocate(trigs)

      deallocate(idelem)
      deallocate(idnode)
      deallocate(ieptr)
      deallocate(ieind)

c 
      stop 9999
1000  format(i10,3e15.5)
1100  format(10i10)
1200  format(i10,e15.5)
1201   format(i10,10e15.5)
1550  format(a,i5,a,a,a,i5)
      end
     










 
 
