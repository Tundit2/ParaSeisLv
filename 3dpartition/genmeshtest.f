      program genmeshtest
      implicit double precision(a-h,o-z)
      dimension coor(100,3),nele(100,9)
      dimension disp(100,3),id(100,3)
      dimension u(100,3),inblk(100),ieblk(100),jnblk(100)
      
c     generate a 3*3*3 elements mesh to test 
      
      dx = 1.
      dy = 1.
      dz = 1.
      
      nx = 3
      ny = 3
      nz = 3
      
      n = 0
      do k = 1,nz+1
      do j = 1,ny+1
      	do i = 1,nx+1
      		n = n+1
      		x = (i-1)*1.*dx
      		y = (j-1)*1.*dy
      		z = (k-1)*1.*dz
      		coor(n,1) = x
      		coor(n,2) = y
      		coor(n,3) = z
      	end do
      end do
      end do
      
      num = 0
      do k = 1,nz
      	do j = 1,ny
      		do i = 1,nx
      			num = num+1
      			nele(num,1) = (k-1)*(nx+1)*(ny+1)+(j-1)*(nx+1)+i
      			nele(num,2) = (k-1)*(nx+1)*(ny+1)+(j-1)*(nx+1)+i+1
      			nele(num,3) = (k-1)*(nx+1)*(ny+1)+j*(nx+1)+i+1
      			nele(num,4) = (k-1)*(nx+1)*(ny+1)+j*(nx+1)+i
      			nele(num,5) = k*(nx+1)*(ny+1)+(j-1)*(nx+1)+i
      			nele(num,6) = k*(nx+1)*(ny+1)+(j-1)*(nx+1)+i+1
      			nele(num,7) = k*(nx+1)*(ny+1)+j*(nx+1)+i+1
      			nele(num,8) = k*(nx+1)*(ny+1)+j*(nx+1)+i
      			nele(num,9) = k
      		end do
      	end do
      end do
      
      do i = 1,n
      	do j = 1,3
      	id(i,j) = 1
        end do
      end do
      
      do k = 1,nz+1
      	do i = 1,nx+1
      		do j = 1,1
      			inod = (k-1)*(nx+1)*(ny+1)+(j-1)*(nx+1)+i
      			id(inod,2) = -1
      		end do
      	end do
      end do
      
      do k = 1,nz+1
      	do i = 1,nx+1
      		do j = ny+1,ny+1
      			inod = (k-1)*(nx+1)*(ny+1)+(j-1)*(nx+1)+i
      			id(inod,2) = -1
      		end do
      	end do
      end do
       
      do k = 1,nz+1
      	do j = 1,ny+1
      		do i = 1,1
      			inod = (k-1)*(nx+1)*(ny+1)+(j-1)*(nx+1)+i
      			id(inod,1) = -1
      		end do
      	end do
      end do	
      
      do k = 1,nz+1
      	do j = 1,ny+1
      		do i = nx+1,nx+1
      			inod = (k-1)*(nx+1)*(ny+1)+(j-1)*(nx+1)+i
      			id(inod,1) = -1
      		end do
      	end do
      end do	
      
      do j = 1,ny+1
      	do i = 1,nx+1
      		inod = (j-1)*(nx+1)+i
      		id(inod,1) = -1
      		id(inod,2) = -1
      		id(inod,3) = -1
      	end do
      end do
      
      do i = 1,n
      	disp(i,1) = i*1.
      	disp(i,2) = i*(-1.)
      	disp(i,3) = (n-i)*1.
      	u(i,1) = 1.
      	u(i,2) = 2.
      	u(i,3) = 3.
      end do
      
c     let us make a simple partition  
      cx = nx*1./2.
      cy = ny*1./2.
      
      do i = 1,n
      	x = coor(i,1)
      	y = coor(i,2)
      	if ((x.le.cx).and.(y.le.cy)) inblk(i) = 0
      	if ((x.le.cx).and.(y.gt.cy)) inblk(i) = 1
      	if ((x.gt.cx).and.(y.le.cy)) inblk(i) = 2
      	if ((x.gt.cx).and.(y.gt.cy)) inblk(i) = 3
      end do
      
      open(11,file='elementpart.dat',form='formatted',
     *  status='unknown')
      open(12,file='nodepart.dat',form='formatted',
     *  status='unknown')
      do ipart = 0,3
      	nn = 0
      	n2 = 0
      	do i = 1,num
      		ieblk(i) = 0
      	end do
      	do i = 1,n
      		jnblk(i) = 0
      	end do
      	do i = 1,num
      		do j = 1,8
      			inod = nele(i,j)
      			if (inblk(inod).eq.ipart) then
      				ieblk(i) = 1
      			end if
      		end do
      		if (ieblk(i).eq.1) nn = nn+1
      		if (ieblk(i).eq.1) then
      		  do j = 1,8
      		  	inod = nele(i,j)
      		  	jnblk(inod) = 1
      		  end do
      		end if
      	end do
      	write(11,1100) nn,9
      	do i = 1,num
      		if (ieblk(i).eq.1) then
          	write(11,1100) i,(nele(i,j),j=1,9)
          end if
        end do
        do i = 1,n
        	if (jnblk(i).eq.1) n2 = n2+1
        end do
        write(12,1100) n2
        do i = 1,n
        	if (jnblk(i).eq.1) then
        	  write(12,1200) i,(coor(i,j),j=1,3)
        	end if
        end do
        
      end do
      close(11)
      close(12)
      
      open(11,file='element.dat',form='formatted',
     *  status='unknown')
      write(11,1100) num,9
      do i = 1,num
      	write(11,1100) i,(nele(i,j),j=1,9)
      end do
      close(11)
      
      open(11,file='node.dat',form='formatted',
     *  status='unknown')
      write(11,1100) n,3
      do i = 1,n
      	write(11,1200) i,(coor(i,j),j=1,3)
      end do
      close(11)
      
      open(11,file='nodeipart.dat',form='formatted',
     *  status='unknown')
      write(11,1100) n,1
      do i = 1,n
      	write(11,1100) i,inblk(i)
      end do
      close(11)
      	
      
      
      
1100  format(15I8)
1200  format(I8,15F15.3)


      end