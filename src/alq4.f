      subroutine alq4(coorr,coefr,prmt,estif,emass,edamp,eload,num)
      implicit real*8 (a-h,o-z)
      dimension estif(12,12),elump(12),emass(12),
     & edamp(12),eload(12)
      dimension prmt(*),
     & coorr(2,4),coor(2)
      common /ralq4/ru(4,12),rv(4,12),rw(4,12),
     & cu(4,3),cv(4,3),cw(4,3)
      common /valq4/rctr(2,2),crtr(2,2)
      common /dalq4/ refc(2,4),gaus(4),
     & nnode,ngaus,ndisp,nrefc,ncoor,nvar,
     & nvard(3),kdord(3),kvord(12,3)
      fx=prmt(1)
      fy=prmt(2)
      fz=prmt(3)
      time=prmt(4)
      dt=prmt(5)
      imate=prmt(6)+0.5
      ielem=prmt(7)+0.5
      nelem=prmt(8)+0.5
      if (num.eq.1) call alq4i
      do 10 i=1,nvar
      emass(i)=0.0
      edamp(i)=0.0
      eload(i)=0.0
      do 10 j=1,nvar
      estif(i,j)=0.0
10    continue
      do 999 igaus=1,ngaus
      call alq4t(nnode,nrefc,ncoor,refc(1,igaus),coor,coorr,
     & rctr,crtr,det)
      x=coor(1)
      y=coor(2)
      rx=refc(1,igaus)
      ry=refc(2,igaus)
      iu=(igaus-1)*3+1
      iv=(igaus-1)*3+1
      iw=(igaus-1)*3+1
      if (num.gt.1) goto 2
      call alq41(refc(1,igaus),ru(1,iu),rctr,crtr)
      call alq42(refc(1,igaus),rv(1,iv),rctr,crtr)
      call alq43(refc(1,igaus),rw(1,iw),rctr,crtr)
2     continue
      call shapn(nrefc,ncoor,4,ru(1,iu),cu,crtr,1,3,3)
      call shapn(nrefc,ncoor,4,rv(1,iv),cv,crtr,1,3,3)
      call shapn(nrefc,ncoor,4,rw(1,iw),cw,crtr,1,3,3)
      weigh=det*gaus(igaus)
      do 202 i=1,4
      iv=kvord(i,1)
      do 201 j=1,4
      jv=kvord(j,1)
      stif=+cu(i,1)*cu(j,1)*0.0
      estif(jv,iv)=estif(jv,iv)+stif*weigh
201    continue
202    continue
      stif= 0.d0
      elump(1)=stif*weigh
      stif= 0.d0
      elump(4)=stif*weigh
      stif= 0.d0
      elump(7)=stif*weigh
      stif= 0.d0
      elump(10)=stif*weigh
      stif= 0.d0
      elump(2)=stif*weigh
      stif= 0.d0
      elump(5)=stif*weigh
      stif= 0.d0
      elump(8)=stif*weigh
      stif= 0.d0
      elump(11)=stif*weigh
      stif= 0.d0
      elump(3)=stif*weigh
      stif= 0.d0
      elump(6)=stif*weigh
      stif= 0.d0
      elump(9)=stif*weigh
      stif= 0.d0
      elump(12)=stif*weigh
      do 301 i=1,nnode
      if (nvard(1).lt.i) goto 301
      iv = kvord(i,1)
      emass(iv)=emass(iv)+elump(iv)*cu(i,1)
      if (nvard(2).lt.i) goto 301
      iv = kvord(i,2)
      emass(iv)=emass(iv)+elump(iv)*cv(i,1)
      if (nvard(3).lt.i) goto 301
      iv = kvord(i,3)
      emass(iv)=emass(iv)+elump(iv)*cw(i,1)
301   continue
      stif= 0.d0
      elump(1)=stif*weigh
      stif= 0.d0
      elump(4)=stif*weigh
      stif= 0.d0
      elump(7)=stif*weigh
      stif= 0.d0
      elump(10)=stif*weigh
      stif= 0.d0
      elump(2)=stif*weigh
      stif= 0.d0
      elump(5)=stif*weigh
      stif= 0.d0
      elump(8)=stif*weigh
      stif= 0.d0
      elump(11)=stif*weigh
      stif= 0.d0
      elump(3)=stif*weigh
      stif= 0.d0
      elump(6)=stif*weigh
      stif= 0.d0
      elump(9)=stif*weigh
      stif= 0.d0
      elump(12)=stif*weigh
      do 401 i=1,nnode
      if (nvard(1).lt.i) goto 401
      iv = kvord(i,1)
      edamp(iv)=edamp(iv)+elump(iv)*cu(i,1)
      if (nvard(2).lt.i) goto 401
      iv = kvord(i,2)
      edamp(iv)=edamp(iv)+elump(iv)*cv(i,1)
      if (nvard(3).lt.i) goto 401
      iv = kvord(i,3)
      edamp(iv)=edamp(iv)+elump(iv)*cw(i,1)
401   continue
      do 501 i=1,4
      iv=kvord(i,1)
      stif=+cu(i,1)*fx
      eload(iv)=eload(iv)+stif*weigh
501   continue
      do 502 i=1,4
      iv=kvord(i,2)
      stif=+cv(i,1)*fy
      eload(iv)=eload(iv)+stif*weigh
502   continue
      do 503 i=1,4
      iv=kvord(i,3)
      stif=+cw(i,1)*fz
      eload(iv)=eload(iv)+stif*weigh
503   continue
999   continue
998   continue
      return
      end

      subroutine alq4i
      implicit real*8 (a-h,o-z)
      common /dalq4/ refc(2,4),gaus(4),
     & nnode,ngaus,ndisp,nrefc,ncoor,nvar,
     & nvard(3),kdord(3),kvord(12,3)
      ngaus=  4
      ndisp=  3
      nrefc=  2
      ncoor=  2
      nvar = 12
      nnode=  4
      kdord(1)=1
      nvard(1)=4
      kvord(1,1)=1
      kvord(2,1)=4
      kvord(3,1)=7
      kvord(4,1)=10
      kdord(2)=1
      nvard(2)=4
      kvord(1,2)=2
      kvord(2,2)=5
      kvord(3,2)=8
      kvord(4,2)=11
      kdord(3)=1
      nvard(3)=4
      kvord(1,3)=3
      kvord(2,3)=6
      kvord(3,3)=9
      kvord(4,3)=12
      refc(1,1)=-1.
      refc(2,1)=-1.
      gaus(1)=1.
      refc(1,2)=1.
      refc(2,2)=-1.
      gaus(2)=1.
      refc(1,3)=1.
      refc(2,3)=1.
      gaus(3)=1.
      refc(1,4)=-1.
      refc(2,4)=1.
      gaus(4)=1.
      end


      subroutine alq4t(nnode,nrefc,ncoor,refc,coor,coorr,rc,cr,det)
      implicit real*8 (a-h,o-z)
      dimension refc(nrefc),rc(ncoor,nrefc),cr(nrefc,ncoor),a(5,10),
     *            coorr(ncoor,nnode),coor(ncoor)
      call talq4(refc,coor,coorr,rc)
      n=nrefc
      m=n*2
      det = 1.0
      do 10 i=1,n
      do 10 j=1,n
      if (i.le.ncoor) a(i,j) = rc(i,j)
      if (i.gt.ncoor) a(i,j)=1.0
      a(i,n+j)=0.0
      if (i.eq.j) a(i,n+i) = 1.0
10    continue
c     write(*,*) 'a ='
c     do 21 i=1,n
c21   write(*,8) (a(i,j),j=1,m)
      do 400 i=1,n
      amax = 0.0
      l = 0
      do 50 j=i,n
      c = a(j,i)
      if (c.lt.0.0) c = -c
      if (c.le.amax) goto 50
      amax = c
      l = j
50    continue
      do 60 k=1,m
      c = a(l,k)
      a(l,k) = a(i,k)
      a(i,k) = c
60    continue
      c = a(i,i)
      det = c*det
      do 100 k=i+1,m
100   a(i,k) = a(i,k)/c
      do 300 j=1,n
      if (i.eq.j) goto 300
      do 200 k=i+1,m
200   a(j,k) = a(j,k)-a(i,k)*a(j,i)
c     write(*,*) 'i =',i,'  j =',j,'  a ='
c     do 11 ii=1,n
c11   write(*,8) (a(ii,jj),jj=1,m)
300   continue
400   continue
      do 500 i=1,nrefc
      do 500 j=1,ncoor
500   cr(i,j) = a(i,n+j)
c     write(*,*) 'a ='
c     do 22 i=1,n
c22   write(*,8) (a(i,j),j=1,m)
c     write(*,*) 'rc ='
c     do 24 i=1,ncoor
c24   write(*,8) (rc(i,j),j=1,nrefc)
c     write(*,*) 'cr ='
c     do 23 i=1,nrefc
c23   write(*,8) (cr(i,j),j=1,ncoor)
c     write(*,*) 'det =',det
      if (det.lt.0.0) det=-det
c     write(*,*) 'det =',det
8     format(1x,6f12.3)
      end

      subroutine alq41(refc,shpr,rctr,crtr)
      implicit real*8 (a-h,o-z)
      dimension refc(2),shpr(4,3),rctr(2,2),crtr(2,2)
      external falq41
      rx=refc(1)
      ry=refc(2)
      call dshap(falq41,refc,shpr,2,4,1)
      return
      end

      real*8 function falq41(refc,n)
      implicit real*8 (a-h,o-z)
      common /ccalq4/ xa(4),ya(4)
      common /valq4/ rctr(2,2),crtr(2,2)
      dimension refc(2)
      common /coord/ coor(3),coora(27,3)
      x=coor(1)
      y=coor(2)
      rx=refc(1)
      ry=refc(2)
      goto (1,2,3,4) n
1     falq41=+(+1.-rx)/2.*(+1.-ry)/2. 
      goto 1000
2     falq41=+(+1.+rx)/2.*(+1.-ry)/2. 
      goto 1000
3     falq41=+(+1.+rx)/2.*(+1.+ry)/2. 
      goto 1000
4     falq41=+(+1.-rx)/2.*(+1.+ry)/2. 
      goto 1000
1000  return
      end

      subroutine alq42(refc,shpr,rctr,crtr)
      implicit real*8 (a-h,o-z)
      dimension refc(2),shpr(4,3),rctr(2,2),crtr(2,2)
      external falq42
      rx=refc(1)
      ry=refc(2)
      call dshap(falq42,refc,shpr,2,4,1)
      return
      end

      real*8 function falq42(refc,n)
      implicit real*8 (a-h,o-z)
      common /ccalq4/ xa(4),ya(4)
      common /valq4/ rctr(2,2),crtr(2,2)
      dimension refc(2)
      common /coord/ coor(3),coora(27,3)
      x=coor(1)
      y=coor(2)
      rx=refc(1)
      ry=refc(2)
      goto (1,2,3,4) n
1     falq42=+(+1.-rx)/2.*(+1.-ry)/2. 
      goto 1000
2     falq42=+(+1.+rx)/2.*(+1.-ry)/2. 
      goto 1000
3     falq42=+(+1.+rx)/2.*(+1.+ry)/2. 
      goto 1000
4     falq42=+(+1.-rx)/2.*(+1.+ry)/2. 
      goto 1000
1000  return
      end

      subroutine alq43(refc,shpr,rctr,crtr)
      implicit real*8 (a-h,o-z)
      dimension refc(2),shpr(4,3),rctr(2,2),crtr(2,2)
      external falq43
      rx=refc(1)
      ry=refc(2)
      call dshap(falq43,refc,shpr,2,4,1)
      return
      end

      real*8 function falq43(refc,n)
      implicit real*8 (a-h,o-z)
      common /ccalq4/ xa(4),ya(4)
      common /valq4/ rctr(2,2),crtr(2,2)
      dimension refc(2)
      common /coord/ coor(3),coora(27,3)
      x=coor(1)
      y=coor(2)
      rx=refc(1)
      ry=refc(2)
      goto (1,2,3,4) n
1     falq43=+(+1.-rx)/2.*(+1.-ry)/2. 
      goto 1000
2     falq43=+(+1.+rx)/2.*(+1.-ry)/2. 
      goto 1000
3     falq43=+(+1.+rx)/2.*(+1.+ry)/2. 
      goto 1000
4     falq43=+(+1.-rx)/2.*(+1.+ry)/2. 
      goto 1000
1000  return
      end

      subroutine talq4(refc,coor,coorr,rc)
      implicit real*8 (a-h,o-z)
      dimension refc(2),coor(2),coorr(2,4),rc(2,2)
      common /ccalq4/ x(4),y(4)
      external ftalq4
      do 100 n=1,4
      x(n)=coorr(1,n)
      y(n)=coorr(2,n)
100   continue
      rx=refc(1)
      ry=refc(2)
      call dcoor(ftalq4,refc,coor,rc,2,2,1)
      return
      end

      real*8 function ftalq4(refc,n)
      implicit real*8 (a-h,o-z)
      dimension refc(2)
      common /ccalq4/ x(4),y(4)
      common /valq4/ rctr(2,2),crtr(2,2)
      rx=refc(1)
      ry=refc(2)
      goto (1,2) n
1     ftalq4=+(+(+1.-rx)/2.*(+1.-ry)/2.)*x(1)+(+(+1.+rx)/
     & 2.*(+1.-ry)/2.)*x(2)+(+(+1.+rx)/2.*(+1.+ry)/2.)*x(3)+(+
     & (+1.-rx)/2.*(+1.+ry)/2.)*x(4)
      goto 1000
2     ftalq4=+(+(+1.-rx)/2.*(+1.-ry)/2.)*y(1)+(+(+1.+rx)/
     & 2.*(+1.-ry)/2.)*y(2)+(+(+1.+rx)/2.*(+1.+ry)/2.)*y(3)+(+
     & (+1.-rx)/2.*(+1.+ry)/2.)*y(4)
      goto 1000
1000  return
      end

