      subroutine mswitch(nsource,msend)
      implicit real*8(a-h,o-z)
      
      nsource = 0
      icheck = 0
      open(21,file='partition.dat',form='formatted',status='old')
      read(21,*) numblk
      close(21)
      do iblk=1,numblk
      call recvint(nsource,iblk,icheck)
      enddo
c      print *,'master icheck is ',icheck 
      if(icheck.eq.0) then
        do iblk=1,numblk
          call sendint(iblk,nsource,msend)
        enddo
        return
      endif

      msend=1
      msendi=0
      do iblk=1,numblk
         call recvint(nsource,iblk,msendi)
         if(msendi.eq.0) msend=0
      enddo
      
      do iblk=1,numblk
         call sendint(iblk,nsource,msend)
      enddo
      
      return
      end

      subroutine sswitch(nsource,iblk,msend,icheck)
      implicit real*8(a-h,o-z)

      call sendint(nsource,iblk,icheck)
      if(icheck.eq.0) then
        call recvint(iblk,nsource,msend)
        return
      endif

      call sendint(nsource,iblk,msend)

      call recvint(iblk,nsource,msend)

      return
      end
