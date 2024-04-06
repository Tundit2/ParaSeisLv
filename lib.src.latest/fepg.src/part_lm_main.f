      subroutine partpre_lm(mdivx,mdivy,mdivz,isignpart,
     *indx,indy,indz,ind,
     *knode,ncoor,nparts,numblk,nedges,
     *kelem,maxa,licensefile,numcol,na,mglnod,
     *idnode,nod_ord,knode_iblk,
     *coor)
      implicit real*8 (a-h,o-z)

c====================================================================
c     idnode:the index of the node belone which iblk
c     knode_iblk:the number of the node in every subdomain
c     N_update:the number of the internal nodes in every subdomain
c     num_iblk:the number of the elements in every subdomain
c     mglnod:the map of node number from global to local
c     mlgelm_no:the map of elem number from local to global
c     node_iblk:the local element array
c     mlgnod:the map of node number from local to global(include the
c            external nodes)
c=====================================================================
      character*22 fname1,fname2
      DIMENSION NODE(kelem),na(maxa),numcol(knode)
      dimension IDNODE(knode)
      dimension mglnod(knode)
      dimension knode_iblk(numblk)
      dimension coor(knode,ncoor)
      dimension nod_ord(knode)
      dimension indx(*),indy(*),indz(*),ind(*)
      character*1 material
      character*22 GraphFile
      character*1000 licensefile
      character*1000 lictest
      lictest=licensefile
c      include 'partdata.h'
c
      if(isignpart.eq.2) then
        numcol(1)=0
c        call verify1(numcol(1),licensefile)
        call verify1(numcol(1),lictest)
        if(numcol(1).ne.1) then
          print *,"Error!wrong license number!",numcol(1)
          print *,"Please contact with FEGENSOFT if you are legal user."
          call abortjob(ierr)
        endif
        call partcoor(knode,mdivx,mdivy,mdivz,coor(1,1),coor(1,2),
     *  coor(1,3),mglnod,nod_ord,knode_iblk,idnode,indx,indy,indz,ind)
      else
C
C      Begin partition the graph
C
       call pmetismain(nparts,idnode,knode,nedges,numcol,na,licensefile)
c       print *,'numcol(1)=',numcol(1)
        numcol(1)=0
        call verify1(numcol(1),lictest)
       if(numcol(1).ne.1) then
         print *,"Error!wrong license number!",numcol(1)
         print *,"Please contact with FEGENSOFT if you are legal user."
         call abortjob(ierr)
       endif
c       numcol(1)=0
      endif
c      print *,'idnode ========'
c      print *,(idnode(i),i=1,knode)
c
       return
       end
      subroutine partpost_lm(mdivx,mdivy,mdivz,isignpart,
     *indx,indy,indz,ind,
     *knode,ncoor,nparts,numblk,nedges,
     *kelem,max_lknode,max_lknode3,max_lkelem,max_lnum,maxa,
     *numtyp,material,licensefile,numcol,lm,na,mglnod,
     *nb_elm,node_iblk,mlgelm_no,num_iblk,mnodei,nnodei,
     *idnode,kelem_iblk,nod_ord,knode_iblk,n_update,mlgnod,
     *iorder,mnode,nnode,iord_nod,itemp,iord,
     *node,nknode_iblk,nn_update,nmlgnod,norder,nmnode,
     *nnnode,niord_nod,coor,coor_iblk)
      implicit real*8 (a-h,o-z)

c====================================================================
c     idnode:the index of the node belone which iblk
c     knode_iblk:the number of the node in every subdomain
c     N_update:the number of the internal nodes in every subdomain
c     num_iblk:the number of the elements in every subdomain
c     mglnod:the map of node number from global to local
c     mlgelm_no:the map of elem number from local to global
c     node_iblk:the local element array
c     mlgnod:the map of node number from local to global(include the
c            external nodes)
c=====================================================================
      character*22 fname1,fname2
      DIMENSION NODE(kelem),na(maxa),numcol(knode),lm(1000)
      dimension IDNODE(knode)
      dimension mnode(numtyp),nnode(numtyp)
      dimension nmnode(numtyp),nnnode(numtyp)
      dimension mglnod(knode)
      dimension knode_iblk(numblk),num_iblk(numblk),kelem_iblk(numblk)
      dimension N_update(numblk),mlgnod(numblk,max_lknode)
      dimension node_iblk(numblk,max_lkelem),mlgelm_no(numblk,max_lnum)
      dimension mnodei(numblk,numtyp),nnodei(numblk,numtyp)
      dimension coor_iblk(max_lknode3),coor(knode,ncoor)
      dimension nb_elm(1000)
      dimension nknode_iblk(numblk)
      dimension nN_update(numblk),nmlgnod(numblk,max_lknode)
      dimension itemp(max_lknode),iord(max_lknode)
      dimension iorder(numblk,max_lknode)
      dimension norder(numblk,max_lknode)
      dimension nod_ord(knode),iord_nod(knode),niord_nod(knode)
      dimension indx(*),indy(*),indz(*),ind(*)
      character*1 material
      character*22 GraphFile
      character*1000 licensefile
      character*1000 lictest
      lictest=licensefile
c      include 'partdata.h'
c
c==========  INITIALIZE some arrays  =============
c
      do j=1,max_lknode
        do iblk=1,numblk
          mlgnod(iblk,j)=0
        enddo
      enddo
      do j=1,max_lkelem
        do iblk=1,numblk
          node_iblk(iblk,j)=0
        enddo
      enddo
      do j=1,max_lnum
        do iblk=1,numblk
          mlgelm_no(iblk,j)=0
        enddo
      enddo
      do iblk=1,numblk
        knode_iblk(iblk)=0
      enddo
      do iblk=1,numblk
        num_iblk(iblk)=0
      enddo
      do iblk=1,numblk
        N_update(iblk)=0
      enddo
      do ityp=1,numtyp
        do iblk=1,numblk
          mnodei(iblk,ityp)=0
        enddo
      enddo
      do ityp=1,numtyp
        do iblk=1,numblk
          nnodei(iblk,ityp)=0
        enddo
      enddo
      do iblk=1,numblk
        kelem_iblk(iblk)=0
      enddo
c==========  Initial finished  ==========
        numcol(1)=0
c        call verify1(numcol(1),licensefile)
        call verify1(numcol(1),lictest)
c        numcol(1)=1
        if(numcol(1).ne.1) then
          print *,"Error!wrong license number!",numcol(1)
          print *,"Please contact with FEGENSOFT if you are legal user."
          call abortjob(ierr)
        endif
C===========================================C
c
c     get the internal node number of the iblk subdomain
c
      do i=1,knode
         nblk=idnode(i)+1
         N_update(nblk)=N_update(nblk)+1
         mlgnod(nblk,N_update(nblk))=i
         mglnod(i)=N_update(nblk)
      enddo
        
      do iblk=1,numblk
         knode_iblk(iblk)=N_update(iblk)
      enddo
c
c     get the new order of all nodes
c
      inod=0
      do iblk=1,numblk
         do i=1,knode_iblk(iblk)
            inod=inod+1
            jnod=mlgnod(iblk,i)
            nod_ord(jnod)=inod
            iord_nod(inod)=jnod
         enddo
      enddo
c      print *,'inod,nod_ord,ord_nod ==========',inod
c      print *,(nod_ord(i),i=1,knode)
c      print *,(iord_nod(i),i=1,knode)
c      open(21,file='order.nod',form='unformatted',status='unknown')
c      write(21) knode
c      write(21) (iord_nod(i),i=1,knode)
c      close(21)
c
c     get the elem number from local to global in every subdomain
c
      do iblk=1,numblk
         num_iblk(iblk)=0
         kelem_iblk(iblk)=0
      enddo
      kelem1=0
      do ityp=1,numtyp
        num=mnode(ityp)
        nnod=nnode(ityp)
        nne=nnod
        if(material.eq.'y'.or.material.eq.'Y') then
           nne=nnod-1
        endif
        do ne=1,num
          do j = 1,nne
             nb_elm(j)=0
          enddo
          nblk_ne=0
c         find the global elem_no
          do j = 1,nne
            nodi = NODE(kelem1+(ne-1)*NNod+J)
            iblk=idnode(nodi)+1
            do k=1,nblk_ne
               if(iblk.eq.nb_elm(k)) goto 100
            enddo
            nblk_ne=nblk_ne+1
            nb_elm(nblk_ne)=iblk
 100        continue
          enddo
c         get the external points and the local node array
          do k=1,nblk_ne
            iblk=nb_elm(k)
            mnodei(iblk,ityp)=mnodei(iblk,ityp)+1
            nskp=kelem_iblk(iblk)
            do inod=1,nne
               nodj=node(kelem1+(ne-1)*nnod+inod)
               if(iblk.eq.idnode(nodj)+1) then
                node_iblk(iblk,nskp+(mnodei(iblk,ityp)-1)*nnod+inod)
     *              =mglnod(nodj)
               else
                do i=N_update(iblk)+1,knode_iblk(iblk)
                   if(nodj.eq.mlgnod(iblk,i)) goto 150
                enddo
                knode_iblk(iblk)=knode_iblk(iblk)+1
                mlgnod(iblk,knode_iblk(iblk))=nodj
                node_iblk(iblk,nskp+(mnodei(iblk,ityp)-1)*nnod+inod)
     *              =knode_iblk(iblk)
 150            continue
                node_iblk(iblk,nskp+(mnodei(iblk,ityp)-1)*nnod+inod)
     *              =i
               endif
            enddo
            if(material.eq.'y'.or.material.eq.'Y') then
              node_iblk(iblk,nskp+mnodei(iblk,ityp)*nnod)
     *           =node(kelem1+ne*nnod)
            endif
            mlgelm_no(iblk,num_iblk(iblk)+mnodei(iblk,ityp))=ne
          end do
        end do
        do iblk=1,numblk
          num_iblk(iblk)=num_iblk(iblk)+mnodei(iblk,ityp)
          nnodei(iblk,ityp)=nnod
          kelem_iblk(iblk)=kelem_iblk(iblk)
     *           +mnodei(iblk,ityp)*nnodei(iblk,ityp)
        enddo
        kelem1=kelem1+num*nnod
      enddo
c
c         Sort the mlgnod
c
      do iblk=1,numblk
         do i=1,max_lknode
            iord(i)=i
         enddo
         do i=1,knode_iblk(iblk)
            itemp(i)=nod_ord(mlgnod(iblk,i))
         enddo
         nleft=0
         nright=knode_iblk(iblk)-1
         call qsort(itemp,iord,nleft,nright)
         do i=1,knode_iblk(iblk)
            itemp(i)=mlgnod(iblk,i)
         enddo
         do i=1,knode_iblk(iblk)
            mlgnod(iblk,i)=itemp(iord(i))
            iorder(iblk,iord(i))=i
         enddo
         do i=1,N_update(iblk)
            lnod=iorder(iblk,i)
            inod=mlgnod(iblk,lnod)
            mglnod(inod)=lnod
         enddo
c
c        change the node array
c
         kelemi=0
         do ityp=1,numtyp
            do ne=1,mnodei(iblk,ityp)
               nnod=nnodei(iblk,ityp)
               nne=nnod
               if(material.eq.'y'.or.material.eq.'Y') nne=nne-1
               do inod=1,nne
                itp=node_iblk(iblk,kelemi+(ne-1)*nnod+inod)
                node_iblk(iblk,kelemi+(ne-1)*nnod+inod)=iorder(iblk,itp)
               enddo
            enddo
            kelemi=kelemi+mnodei(iblk,ityp)*nnodei(iblk,ityp)
         enddo
         do i=1,knode_iblk(iblk)
            if(iord(i).le.N_update(iblk)) then
               iorder(iblk,i)=1
            else
               iorder(iblk,i)=0
            endif
         enddo
      enddo
c-----------------------------------------------------------------
c     get the external nodes in every subdomain
c
c      kelem1=0
c      do iblk=1,numblk
c         num_iblk(iblk)=0
c      enddo
c      do ityp=1,numtyp
c         do iblk=1,numblk
c            num=mnodei(iblk,ityp)
c            nnod=nnodei(iblk,ityp)
c            nne=nnod
c            if(material.eq.'y'.or.material.eq.'Y') nne=nnod-1
c            do ne=1,num
c               no_elm=mlgelm_no(iblk,num_iblk(iblk)+ne)
c               do inod=1,nne
c                  nodi=node(kelem1+(no_elm-1)*nnod+inod)
c                  if(idnode(nodi)+1.eq.iblk) goto 300
c                  do i=N_update(iblk)+1,knode_iblk(iblk)
c                     if(nodi.eq.mlgnod(iblk,i)) goto 300
c                  enddo
c                  knode_iblk(iblk)=knode_iblk(iblk)+1
c                  mlgnod(iblk,knode_iblk(iblk))=nodi
c 300              continue
c               enddo
c            enddo
c            num_iblk(iblk)=num_iblk(iblk)+num
c         enddo
c         kelem1=kelem1+mnode(ityp)*nnode(ityp)
c      enddo
c
       return
       end
