c
c     the send and recv subroutines for node displacement .
c     m* are the subroutine for master program.
c     s* are the subroutine for slave program.
c
      subroutine mrecvu2d(nsource,numblk,knode,kdgof,apool,u,
     * knode_iblk,mlgnod,iorder)
      implicit real*8 (a-h,o-z)
      dimension u(knode,kdgof),apool(*)
      dimension knode_iblk(numblk)
      dimension mlgnod(numblk,*)
      dimension iorder(numblk,*)
c
c     receive displace at every points from iblk subdomain and set vector u
c
      neqmax=0
      do iblk=1,numblk
        call recvar(nsource,iblk,apool,knode_iblk(iblk)*kdgof)
        do i=1,knode_iblk(iblk)
          if(iorder(iblk,i).eq.1) then
             do j=1,kdgof
               neqmax=neqmax+1
               u(mlgnod(iblk,i),j)=apool((j-1)*knode_iblk(iblk)+i)
            enddo
          endif
        enddo
      enddo
      if(neqmax.ne.knode*kdgof) then
         print *,"error! different number of neqmax at recvu2d!!"
      endif
       
      end

      subroutine msendu2d(nsource,numblk,knode,kdgof,apool,u,
     * knode_iblk,mlgnod,iorder)
      implicit real*8 (a-h,o-z)
      dimension u(knode,kdgof),apool(*)
      dimension knode_iblk(numblk)
      dimension mlgnod(numblk,*)
      dimension iorder(numblk,*)
c
c     send displace at every points to iblk subdomain
c
      do iblk=1,numblk
        do j=1,kdgof
          do i=1,knode_iblk(iblk)
             apool((j-1)*knode_iblk(iblk)+i)=u(mlgnod(iblk,i),j)
          enddo
        enddo
        call sendar(iblk,nsource,apool,knode_iblk(iblk)*kdgof)
      enddo

      return
      end

      subroutine mrecvu1d(nsource,numblk,knode,apool,u,
     * knode_iblk,mlgnod,iorder)
      implicit real*8 (a-h,o-z)
      dimension u(knode),apool(*)
      dimension knode_iblk(numblk)
      dimension mlgnod(numblk,*)
      dimension iorder(numblk,*)
c
c     receive displace at every points from iblk subdomain and set vector u
c
      neqmax=0
      do iblk=1,numblk
        call recvar(nsource,iblk,apool,knode_iblk(iblk))
        do i=1,knode_iblk(iblk)
          if(iorder(iblk,i).eq.1) then
            neqmax=neqmax+1
            u(mlgnod(iblk,i))=apool(i)
          endif
        enddo
      enddo
      if(neqmax.ne.knode) then
         print *,"error! different number of neqmax at recvu1d!!"
      endif
       
      end

      subroutine msendu1d(nsource,numblk,knode,apool,u,
     * knode_iblk,mlgnod,iorder)
      implicit real*8 (a-h,o-z)
      dimension u(knode),apool(*)
      dimension knode_iblk(numblk)
      dimension mlgnod(numblk,*)
      dimension iorder(numblk,*)
c
c     send displace at every points to iblk subdomain
c
      do iblk=1,numblk
        do i=1,knode_iblk(iblk)
           apool(i)=u(mlgnod(iblk,i))
        enddo
        call sendar(iblk,nsource,apool,knode_iblk(iblk))
      enddo

      return
      end
c
c     s* are the subroutines of slave programs.
c
      subroutine ssendu2d(nsource,iblk,knode,kdgof,u)
      implicit real*8 (a-h,o-z)
      dimension u(knode,kdgof)
c
c     send node displaces of iblk subdomain to master  
c
      call sendar(nsource,iblk,u,knode*kdgof)

      return
      end
c       
      subroutine srecvu2d(nsource,iblk,knode,kdgof,u)
      implicit real*8 (a-h,o-z)
      dimension u(knode,kdgof)
c
c     send node displaces of iblk subdomain to master  
c
      call recvar(iblk,nsource,u,knode*kdgof)

      return
      end
c       
      subroutine ssendu1d(nsource,iblk,knode,u)
      implicit real*8 (a-h,o-z)
      dimension u(knode)
c
c     send node displaces of iblk subdomain to master  
c
      call sendar(nsource,iblk,u,knode)

      return
      end
c       
      subroutine srecvu1d(nsource,iblk,knode,u)
      implicit real*8 (a-h,o-z)
      dimension u(knode)
c
c     send node displaces of iblk subdomain to master  
c
      call recvar(iblk,nsource,u,knode)

      return
      end
c  
