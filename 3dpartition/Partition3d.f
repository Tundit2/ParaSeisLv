      program partition
      implicit real*8 (a-h,o-z)
      
      dimension coor(1500000,3),node(1500000,9)
      dimension idelem(1500000),idnode(1500000)
      dimension lidelem(150000),lidnode(150000)
      
      
c     open the coor file and element file 
      open(10,file='coor0',
     *  form='unformatted',status='old')
      read(10) knode,kdgof,((coor(i,j),j=1,3),i=1,knode)
      close(10)
      
      open(10,file='elem0',
     *  form='unformatted',status='old')
      read(10) num,nne,((node(i,j),j=1,9),i=1,num)
      close(10)
      
      open(10,file='layerinfo',
     *  form='formatted',status='old')
      read(10,*)
      read(10,*) nlayer,nx,ny
      close(10)
      
      open(10,file='idelem',
     *  form='formatted',status='old')
      read(10,*) lnum
      do i = 1,lnum
      	read(10,*) lidelem(i)
      end do
      close(10)
      
      open(10,file='idnode',
     *  form='formatted',status='old')
      read(10,*) lnode
      do i = 1,lnode
      	read(10,*) lidnode(i)
      end do
      close(10)
      
      knode1 = nlayer*lnode
      num1 = (nlayer-1)*lnum
      if (knode .ne. knode1) then
      	print*, 'Number of nodes cant match!'
      	print*, knode,knode1
      	stop 1111
      end if
      if (num .ne. num1) then
      	print*, 'Number of elements cant match!'
      	print*, num,num1
      	stop 2222
      end if
      
c     lets project the id to 3d elements and nodes      
      do ilayer = 1,nlayer
      	nadd = (ilayer-1)*(nx+1)*(ny+1)
      	do i = 1,lnode
      		inode = i+nadd
      		idnode(inode) = lidnode(i)
      	end do
      end do
      
      do ilayer = 1,nlayer-1
      	nadd = (ilayer-1)*nx*ny
      	do i = 1,lnum
      		inum = i+nadd
      		idelem(inum) = lidelem(i)
      	end do
      end do
      
c     print data to check 
      open(10,file='3didelem',form='formatted',status='unknown')
      write(10,1100) num,nne
      do i = 1,num
      	write(10,1100) i,(node(i,j),j=1,8),idelem(i)
      end do
      close(10)
      open(10,file='3didnode',form='formatted',status='unknown')
      write(10,1100) knode,kdgof
      do i = 1,knode
      	write(10,1200) i,(coor(i,j),j=1,3),idnode(i)
      end do
      close(10)
      
      
c     write GID form to visualization
      open(30,file='partition.flavia.msh',status='unknown',
     +        form='formatted')
        write(30,*) 'Mesh "Whole" Dimension 3',
     +  ' Elemtype Hexahedra Nnode 8'
        write(30,*) 'Coordinates'
        do i = 1,knode
        	write(30,1000) i,(coor(i,j),j=1,3)
        end do
        write(30,*) 'End coordinates'
        write(30,*) 'Elements'
        do i = 1,num
        	write(30,1100) i,(node(i,j),j=1,8),idelem(i)
        end do
        write(30,*)'End elements'
        close(30)
      
      
1000  format(i10,3e15.5)
1100  format(15i10)
1200  format(i10,3f15.5,i10)


      end