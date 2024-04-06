      program mesh
      implicit real*8(a-h,o-z)
      dimension coor(2000000,3),nele(2000000,9)
      dimension coort(2000000,3),nelet(2000000,9)
      dimension disp(2000000,3),id(2000000,3),dispt(2000000,3)
      dimension nodvar(2000000,3),idt(2000000,3)
      dimension u(2000000,3),inblk(2000000),ieblk(2000000)
      dimension jnblk(2000000)
      dimension emate(10,7),nodeall(2000000),nid(2000000)
      
c
      open(10,file='id0',form='unformatted',status='old')
      read(10) numnod,noddof,((id(i,j),j=1,noddof),i=1,numnod)
      close(10)
      
      open(10,file='disp0',form='unformatted',status='old')
      read(10) numnod,noddof,((disp(i,j),j=1,noddof),i=1,numnod)
      close(10)
      
      open(10,file='time0',form='formatted',status='old')
      read(10,*) t0,tmax,dt
      close(10)
      
      open(10,file='coor0',form='unformatted',status='old')
      read(10) numnod,ncoor,((coor(i,j),j=1,ncoor),i=1,numnod)
      close(10)
      
      open(10,file='elem0',form='unformatted',status='old')
      read(10) num,nnode,((nele(i,j),j=1,nnode),i=1,num)
      read(10) nummat,mmat,((emate(i,j),j=1,mmat),i=1,nummat)
      close(10)
      
      open(10,file='3didnode',form='formatted',status='old')
      read(10,*) knode
      do i = 1,knode
      	read(10,*) iskip, rskip, rskip, rskip, inblk(i)
      end do
      close(10)
      
      open(10,file='3didelem',form='formatted',status='old')
      read(10,*) num,nne
      do i = 1,num
      	read(10,*) iskip,iskip1,iskip2,iskip3,iskip4,iskip5,iskip6,
     *   iskip7,iskip8,ieblk(i)
      end do
      close(10)
      
      numblk = 64
      numtyp = 1  
      knodeall = knode
      numnode = knode
      kdgof = 3
      ncoor = 3
      idisp1 = 0
      idisp2 = 0
      open(10,file='mlmddm',form='unformatted',status='unknown')
     	
      	
      open(30,file='mcoor0',form='unformatted',
     *  status='unknown')
      open(31,file='mid0',form='unformatted',
     *  status='unknown')
      open(32,file='mdisp0',form='unformatted',
     *  status='unknown')
      open(33,file='mdisp1',form='unformatted',
     *  status='unknown')
      open(35,file='melem0',form='unformatted',
     *  status='unknown')
      open(36,file='nodeall0',form='unformatted',
     *  status='unknown')
      
      open(130,file='mcoor0.dat',form='formatted',
     *  status='unknown')
      open(131,file='mid0.dat',form='formatted',
     *  status='unknown')
      open(132,file='mdisp0.dat',form='formatted',
     *  status='unknown')
      open(133,file='mdisp1.dat',form='formatted',
     *  status='unknown')
      open(135,file='melem0.dat',form='formatted',
     *  status='unknown')
      open(136,file='nodeall0.dat',form='formatted',
     *  status='unknown')
     
      do ipart = 0,numblk-1
        print*, 'ipart=',ipart
        nn = 0
        n2 = 0
      	do i = 1,num
      		ieblk(i) = 0
      	end do
      	do i = 1,knodeall
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
      	print*, nn

c
        nnt = 0
        do i = 1,knodeall
        	if (jnblk(i).eq.1) then
        		nnt = nnt+1
        	  do j = 1,3
        	  	coort(nnt,j) = coor(i,j)
        	  	idt(nnt,j) = id(i,j)
        	  	dispt(nnt,j) = disp(i,j)
        	  end do
        	  nodeall(nnt) = i
        	  if (inblk(i).eq.ipart) then
        	  	nid(nnt) = 1
        	  	else
        	  	nid(nnt) = -1
        	  end if
        	end if
        end do

c
        net = 0
      	do i = 1,num
      		if (ieblk(i).eq.1) then
      		net = net+1
          	do j = 1,8
          		noyes = 0
          		inodl = nele(i,j)
          		do k = 1,nnt
          			if (nodeall(k).eq.inodl) then
          				nelet(net,j) = k
          				noyes = 1
          			end if
          		end do
          		if (noyes .eq. 0) then
          			print*, 'node cant match'
          			pause 111
          		end if
          	end do
          	
          	nelet(net,9) = nele(i,9)
          end if
        end do
        
        print*, 'Now begin to write cal file...'       
        
        write(30) nnt,3
        write(30) ((coort(i,j),j=1,3),i=1,nnt)
        
        write(31) nnt,3
        write(31) ((idt(i,j),j=1,3),i=1,nnt)
        
        write(32) nnt,3
        write(32) ((dispt(i,j),j=1,3),i=1,nnt)
        
        write(33) nnt,3
        write(33) ((dispt(i,j),j=1,3),i=1,nnt)
        
        write(35) net,9
        write(35) ((nelet(i,j),j=1,9),i=1,net)
        write(35) nummat,mmat
        write(35) ((emate(i,j),j=1,mmat),i=1,nummat)
        
        write(36) nnt
        write(36) (nodeall(i),i=1,nnt)
        write(36) (nid(i),i=1,nnt)
        
         write(130,*) nnt,3
        write(130,*) ((coort(i,j),j=1,3),i=1,nnt)
        
        write(131,*) nnt,3
        write(131,*) ((idt(i,j),j=1,3),i=1,nnt)
        
        write(132,*) nnt,3
        write(132,*) ((dispt(i,j),j=1,3),i=1,nnt)
        
        write(133,*) nnt,3
        write(133,*) ((dispt(i,j),j=1,3),i=1,nnt)
        
        write(135,*) net,9
        write(135,*) ((nelet(i,j),j=1,9),i=1,net)
        write(135,*) nummat,mmat
        write(135,*) ((emate(i,j),j=1,mmat),i=1,nummat)
        
        write(136,*) nnt
        write(136,*) (nodeall(i),i=1,nnt)
        write(136,*) (nid(i),i=1,nnt)
        
        numblkt = numblk
        numtypt = numtyp
        knodet = nnt
        kdgoft = kdgof
        nummatt = nummat
        mmatt = mmat
        ncoort = ncoor
        numt = net
        nnet = nne
        idisp1t = idisp1
        idisp2t = idisp2
        
        write(10) numblkt,numtypt,numnode,knodet,kdgoft,
     &   numt,nnet,nummatt,mmatt,ncoort,idisp1t,idisp2t,t0,tmax,dt
        print*, numblkt,numtypt,numnode,knodet,kdgoft,
     &   numt,nnet,nummatt,mmatt,ncoort,idisp1t,idisp2t,t0,tmax,dt
        
      end do
      close(30)
      close(31)
      close(32)
      close(33)
      close(35)
      close(36)
      
      close(130)
      close(131)
      close(132)
      close(133)
      close(135)
      close(136)
      
      close(10) 
      
      			   
      	
      	
      
      
      
1100  format(15I8)
1200  format(I8,15F15.3)


      end
