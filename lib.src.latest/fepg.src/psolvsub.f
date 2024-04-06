       subroutine callpsolver(maxa,n_update0,n_external,n_dataorg,
     &                    n_update,nupdate,nbindx,val,b,x,ndata_org,
     &                    nexternal,nupdate_index,nextern_index,
     &                    noptions,params)
c
       include "az_aztecf.h"
       include "mpif.h"
c
       integer nproc_config(0:AZ_PROC_SIZE), noptions(0:AZ_OPTIONS_SIZE)
       double precision params(0:AZ_PARAMS_SIZE)
       double precision status(0:AZ_STATUS_SIZE)
       integer n_update,n_update0,n_external,n_dataorg,ierror
       integer i,maxa
       integer n
c
c             See Aztec User's Guide for the variables that follow:
c
       integer nbindx(0:maxa)
       double precision val(0:maxa)
       double precision b(0:n_update0),x(0:n_update0)
c       double precision b(0:n_update0),x(0:n_update0),x1(0:n_update0)
       integer ndata_org(0:n_dataorg)
       integer nupdate(0:n_update0), nexternal(0:n_external)
       integer nupdate_index(0:n_update0), nextern_index(0:n_external)
c       integer nrpntr(0:neqmax)
c
c      get number of processors and the name of this processor
c
       do i = 0,AZ_PROC_SIZE
       nproc_config(i)=0
       end do
       do i = 0,n_update0
       x(i)=0.0d0
       end do
c       do i = 0,n_update0
c       x1(i)=0.0d0
c       end do
       do i = 0,n_dataorg
       ndata_org(i)=0
       end do
       do i = 0,n_external
       nexternal(i)=0
       end do
       do i = 0,n_update0
       nupdate_index(i)=0
       end do
       do i = 0,n_external
       nextern_index(i)=0
       end do
c       do i = 0,neqmax
c       nrpntr(i)=0
c       end do
c
       call AZ_set_proc_config(nproc_config, MPI_COMM_WORLD)
c
c       nrow = N_update
c       nrow = neqall*neqall
c       nrow = neqall
c       call AZ_read_update(N_update,nupdate,nproc_config,nrow,1,0)
c
c      convert matrix to a local distributed matrix */
c
c       if(nproc_config(AZ_PROC_SIZE-5) .eq. 2) then
c       print *,'nproc_config =====',nproc_config
c       print *, 'N_update=======',N_update
c       print *,'N_update == ',N_update
c       print *, 'nupdate=========',(nupdate(i),i=0,N_update-1)
c       print *, 'val============',(val(i),i=0,maxa)
c       print *, 'nbindx==========',(nbindx(i),i=0,maxa)
c       print *, 'new f file ====='
c       print *, (b(I),I=0,N_update-1)
c       print *,'n_external == ',n_external
c       print *, 'nexternal==========',(nexternal(i),i=0,n_external)
c       print *,'n_update0== ',n_update0
c       print *, 'nupdate_index========',(nupdate_index(i),i=0,n_update0)
c       print *, 'nextern_index======',(nextern_index(i),i=0,n_external)
c       print *,'n_dataorg == ',n_dataorg
c       print *, 'ndata_org==========',(ndata_org(i),i=0,n_dataorg)
c       endif
       call AZ_transform(nproc_config,nexternal,nbindx,val,nupdate,
     $                   nupdate_index,nextern_index,ndata_org,
     $                   n_update,0,0,0,0,AZ_MSR_MATRIX)
c
c      Set rhs (delta function at grid center) and initialize guess
c
c      call AZ_reorder_vec(b,ndata_org,nupdate_index,nrpntr)
      call AZ_reorder_vec(b,ndata_org,nupdate_index,0)
       do i = 0, n_update0
          x(nupdate_index(i)) = 0.0d0
       enddo
c
c      solve the system of equations using b  as the right hand side
c
       call AZ_solve(x,b, noptions, params, 0,nbindx,0,0,
     $               0,val, ndata_org, status, nproc_config)
c
       do i = 0,n_update0
         b(i)=0.0d0
       end do
       call AZ_invorder_vec(x,ndata_org,nupdate_index,0,b)
c        print *,(b(i),i=0,N_update-1)
       do i = 0,n_update0
         x(i) = b(i)
       end do
 
       return
       end
 
 
 
 
