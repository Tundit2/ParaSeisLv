      subroutine nuc8g2(coorr,coefr,prmt,estif,emass,edamp,eload,num)
      implicit real*8 (a-h,o-z)
      dimension estif(24,24),elump(24),emass(24),
     &edamp(24),eload(24)
      dimension prmt(*),
     & efuna(24),efunb(24),efunc(24),efund(24),
     & efune(24),efunf(24),coorr(3,8),coor(3)
      common /rnuc8g2/ru(8,32),rv(8,32),rw(8,32),
     & cu(8,4),cv(8,4),cw(8,4)
      common /vnuc8g2/rctr(3,3),crtr(3,3)
      common /dnuc8g2/ refc(3,8),gaus(8),
     & nnode,ngaus,ndisp,nrefc,ncoor,nvar,
     & nvard(3),kdord(3),kvord(24,3)
      pe = prmt(1)
      pv = prmt(2)
      fact = pe/(1.+pv)/(1.-2.*pv)
      fu = prmt(3)
      fv = prmt(4)
      fw = prmt(5)
      rou = prmt(6)
      alpha = prmt(7)
      if (num.eq.1) call nuc8g2i
      do 10 i=1,nvar
      emass(i)=0.0
      edamp(i)=0.0
      eload(i)=0.0
      do 10 j=1,nvar
      estif(i,j)=0.0
10    continue
      do 999 igaus=1,ngaus
      call nuc8g2t(nnode,nrefc,ncoor,refc(1,igaus),coor,coorr,
     & rctr,crtr,det)
      x=coor(1)
      y=coor(2)
      z=coor(3)
      rx=refc(1,igaus)
      ry=refc(2,igaus)
      rz=refc(3,igaus)
      iu=(igaus-1)*4+1
      iv=(igaus-1)*4+1
      iw=(igaus-1)*4+1
      if (num.gt.1) goto 2
      call nuc8g21(refc(1,igaus),ru(1,iu),rctr,crtr)
      call nuc8g22(refc(1,igaus),rv(1,iv),rctr,crtr)
      call nuc8g23(refc(1,igaus),rw(1,iw),rctr,crtr)
2     continue
      call shapn(nrefc,ncoor,8,ru(1,iu),cu,crtr,1,4,4)
      call shapn(nrefc,ncoor,8,rv(1,iv),cv,crtr,1,4,4)
      call shapn(nrefc,ncoor,8,rw(1,iw),cw,crtr,1,4,4)
      weigh=det*gaus(igaus)
      do 100 i=1,24
      efuna(i) = 0.0
      efunb(i) = 0.0
      efunc(i) = 0.0
      efund(i) = 0.0
      efune(i) = 0.0
      efunf(i) = 0.0
100   continue
      do 101 i=1,8
      iv=kvord(i,1)
      stif=+cu(i,2) 
      efuna(iv)=efuna(iv)+stif
101   continue
      do 102 i=1,8
      iv=kvord(i,2)
      stif=+cv(i,3) 
      efunb(iv)=efunb(iv)+stif
102   continue
      do 103 i=1,8
      iv=kvord(i,3)
      stif=+cw(i,4) 
      efunc(iv)=efunc(iv)+stif
103   continue
      do 104 i=1,8
      iv=kvord(i,2)
      stif=+cv(i,4) 
      efund(iv)=efund(iv)+stif
104   continue
      do 105 i=1,8
      iv=kvord(i,3)
      stif=+cw(i,3) 
      efund(iv)=efund(iv)+stif
105   continue
      do 106 i=1,8
      iv=kvord(i,1)
      stif=+cu(i,4) 
      efune(iv)=efune(iv)+stif
106   continue
      do 107 i=1,8
      iv=kvord(i,3)
      stif=+cw(i,2) 
      efune(iv)=efune(iv)+stif
107   continue
      do 108 i=1,8
      iv=kvord(i,1)
      stif=+cu(i,3) 
      efunf(iv)=efunf(iv)+stif
108   continue
      do 109 i=1,8
      iv=kvord(i,2)
      stif=+cv(i,2) 
      efunf(iv)=efunf(iv)+stif
109   continue
      do 202 iv=1,24
      do 201 jv=1,24
      stif=+efuna(iv)*efuna(jv)*(pe+2.d0*pv)
     & +efuna(iv)*efunb(jv)*pe
     & +efuna(iv)*efunc(jv)*pe
     & +efunb(iv)*efuna(jv)*pe
     & +efunb(iv)*efunb(jv)*(pe+2.d0*pv)
     & +efunb(iv)*efunc(jv)*pe
     & +efunc(iv)*efuna(jv)*pe
     & +efunc(iv)*efunb(jv)*pe
     & +efunc(iv)*efunc(jv)*(pe+2.d0*pv)
     & +efund(iv)*efund(jv)*pv
     & +efune(iv)*efune(jv)*pv
     & +efunf(iv)*efunf(jv)*pv
      estif(iv,jv)=estif(iv,jv)+stif*weigh
201    continue
202    continue
      stif=rou
      elump(1)=stif*weigh
      stif=rou
      elump(4)=stif*weigh
      stif=rou
      elump(7)=stif*weigh
      stif=rou
      elump(10)=stif*weigh
      stif=rou
      elump(13)=stif*weigh
      stif=rou
      elump(16)=stif*weigh
      stif=rou
      elump(19)=stif*weigh
      stif=rou
      elump(22)=stif*weigh
      stif=rou
      elump(2)=stif*weigh
      stif=rou
      elump(5)=stif*weigh
      stif=rou
      elump(8)=stif*weigh
      stif=rou
      elump(11)=stif*weigh
      stif=rou
      elump(14)=stif*weigh
      stif=rou
      elump(17)=stif*weigh
      stif=rou
      elump(20)=stif*weigh
      stif=rou
      elump(23)=stif*weigh
      stif=rou
      elump(3)=stif*weigh
      stif=rou
      elump(6)=stif*weigh
      stif=rou
      elump(9)=stif*weigh
      stif=rou
      elump(12)=stif*weigh
      stif=rou
      elump(15)=stif*weigh
      stif=rou
      elump(18)=stif*weigh
      stif=rou
      elump(21)=stif*weigh
      stif=rou
      elump(24)=stif*weigh
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
      stif=rou*alpha
      elump(1)=stif*weigh
      stif=rou*alpha
      elump(4)=stif*weigh
      stif=rou*alpha
      elump(7)=stif*weigh
      stif=rou*alpha
      elump(10)=stif*weigh
      stif=rou*alpha
      elump(13)=stif*weigh
      stif=rou*alpha
      elump(16)=stif*weigh
      stif=rou*alpha
      elump(19)=stif*weigh
      stif=rou*alpha
      elump(22)=stif*weigh
      stif=rou*alpha
      elump(2)=stif*weigh
      stif=rou*alpha
      elump(5)=stif*weigh
      stif=rou*alpha
      elump(8)=stif*weigh
      stif=rou*alpha
      elump(11)=stif*weigh
      stif=rou*alpha
      elump(14)=stif*weigh
      stif=rou*alpha
      elump(17)=stif*weigh
      stif=rou*alpha
      elump(20)=stif*weigh
      stif=rou*alpha
      elump(23)=stif*weigh
      stif=rou*alpha
      elump(3)=stif*weigh
      stif=rou*alpha
      elump(6)=stif*weigh
      stif=rou*alpha
      elump(9)=stif*weigh
      stif=rou*alpha
      elump(12)=stif*weigh
      stif=rou*alpha
      elump(15)=stif*weigh
      stif=rou*alpha
      elump(18)=stif*weigh
      stif=rou*alpha
      elump(21)=stif*weigh
      stif=rou*alpha
      elump(24)=stif*weigh
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
      do 501 i=1,8
      iv=kvord(i,1)
      stif=+cu(i,1)*fu
      eload(iv)=eload(iv)+stif*weigh
501   continue
      do 502 i=1,8
      iv=kvord(i,2)
      stif=+cv(i,1)*fv
      eload(iv)=eload(iv)+stif*weigh
502   continue
      do 503 i=1,8
      iv=kvord(i,3)
      stif=+cw(i,1)*fw
      eload(iv)=eload(iv)+stif*weigh
503   continue
999   continue
      return
      end

      subroutine nuc8g2i
      implicit real*8 (a-h,o-z)
      common /dnuc8g2/ refc(3,8),gaus(8),
     & nnode,ngaus,ndisp,nrefc,ncoor,nvar,
     & nvard(3),kdord(3),kvord(24,3)
      ngaus=  8
      ndisp=  3
      nrefc=  3
      ncoor=  3
      nvar = 24
      nnode=  8
      kdord(1)=1
      nvard(1)=8
      kvord(1,1)=1
      kvord(2,1)=4
      kvord(3,1)=7
      kvord(4,1)=10
      kvord(5,1)=13
      kvord(6,1)=16
      kvord(7,1)=19
      kvord(8,1)=22
      kdord(2)=1
      nvard(2)=8
      kvord(1,2)=2
      kvord(2,2)=5
      kvord(3,2)=8
      kvord(4,2)=11
      kvord(5,2)=14
      kvord(6,2)=17
      kvord(7,2)=20
      kvord(8,2)=23
      kdord(3)=1
      nvard(3)=8
      kvord(1,3)=3
      kvord(2,3)=6
      kvord(3,3)=9
      kvord(4,3)=12
      kvord(5,3)=15
      kvord(6,3)=18
      kvord(7,3)=21
      kvord(8,3)=24
      refc(1,1)=5.77350e-001
      refc(2,1)=5.77350e-001
      refc(3,1)=5.77350e-001
      gaus(1)=1.00000e+000
      refc(1,2)=5.77350e-001
      refc(2,2)=5.77350e-001
      refc(3,2)=-5.77350e-001
      gaus(2)=1.00000e+000
      refc(1,3)=5.77350e-001
      refc(2,3)=-5.77350e-001
      refc(3,3)=5.77350e-001
      gaus(3)=1.00000e+000
      refc(1,4)=5.77350e-001
      refc(2,4)=-5.77350e-001
      refc(3,4)=-5.77350e-001
      gaus(4)=1.00000e+000
      refc(1,5)=-5.77350e-001
      refc(2,5)=5.77350e-001
      refc(3,5)=5.77350e-001
      gaus(5)=1.00000e+000
      refc(1,6)=-5.77350e-001
      refc(2,6)=5.77350e-001
      refc(3,6)=-5.77350e-001
      gaus(6)=1.00000e+000
      refc(1,7)=-5.77350e-001
      refc(2,7)=-5.77350e-001
      refc(3,7)=5.77350e-001
      gaus(7)=1.00000e+000
      refc(1,8)=-5.77350e-001
      refc(2,8)=-5.77350e-001
      refc(3,8)=-5.77350e-001
      gaus(8)=1.00000e+000
      end


      subroutine nuc8g2t(nnode,nrefc,ncoor,refc,coor,coorr,rc,cr,det)
      implicit real*8 (a-h,o-z)
      dimension refc(nrefc),rc(ncoor,nrefc),cr(nrefc,ncoor),a(5,10),
     *            coorr(ncoor,nnode),coor(ncoor)
      call tnuc8g2(refc,coor,coorr,rc)
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

      subroutine nuc8g21(refc,shpr,rctr,crtr)
      implicit real*8 (a-h,o-z)
      dimension refc(3),shpr(8,4),rctr(3,3),crtr(3,3)
      external fnuc8g21
      rx=refc(1)
      ry=refc(2)
      rz=refc(3)
      call dshap(fnuc8g21,refc,shpr,3,8,1)
      return
      end

      real*8 function fnuc8g21(refc,n)
      implicit real*8 (a-h,o-z)
      common /ccnuc8g2/ xa(8),ya(8),za(8)
      common /vnuc8g2/ rctr(3,3),crtr(3,3)
      dimension refc(3)
      common /coord/ coor(3),coora(27,3)
      x=coor(1)
      y=coor(2)
      z=coor(3)
      rx=refc(1)
      ry=refc(2)
      rz=refc(3)
      goto (1,2,3,4,5,6,7,8) n
1     fnuc8g21=+(+1.-rx)/2.*(+1.-ry)/2.*(+1.-rz)/2. 
      goto 1000
2     fnuc8g21=+(+1.+rx)/2.*(+1.-ry)/2.*(+1.-rz)/2. 
      goto 1000
3     fnuc8g21=+(+1.+rx)/2.*(+1.+ry)/2.*(+1.-rz)/2. 
      goto 1000
4     fnuc8g21=+(+1.-rx)/2.*(+1.+ry)/2.*(+1.-rz)/2. 
      goto 1000
5     fnuc8g21=+(+1.-rx)/2.*(+1.-ry)/2.*(+1.+rz)/2. 
      goto 1000
6     fnuc8g21=+(+1.+rx)/2.*(+1.-ry)/2.*(+1.+rz)/2. 
      goto 1000
7     fnuc8g21=+(+1.+rx)/2.*(+1.+ry)/2.*(+1.+rz)/2. 
      goto 1000
8     fnuc8g21=+(+1.-rx)/2.*(+1.+ry)/2.*(+1.+rz)/2. 
      goto 1000
1000  return
      end

      subroutine nuc8g22(refc,shpr,rctr,crtr)
      implicit real*8 (a-h,o-z)
      dimension refc(3),shpr(8,4),rctr(3,3),crtr(3,3)
      external fnuc8g22
      rx=refc(1)
      ry=refc(2)
      rz=refc(3)
      call dshap(fnuc8g22,refc,shpr,3,8,1)
      return
      end

      real*8 function fnuc8g22(refc,n)
      implicit real*8 (a-h,o-z)
      common /ccnuc8g2/ xa(8),ya(8),za(8)
      common /vnuc8g2/ rctr(3,3),crtr(3,3)
      dimension refc(3)
      common /coord/ coor(3),coora(27,3)
      x=coor(1)
      y=coor(2)
      z=coor(3)
      rx=refc(1)
      ry=refc(2)
      rz=refc(3)
      goto (1,2,3,4,5,6,7,8) n
1     fnuc8g22=+(+1.-rx)/2.*(+1.-ry)/2.*(+1.-rz)/2. 
      goto 1000
2     fnuc8g22=+(+1.+rx)/2.*(+1.-ry)/2.*(+1.-rz)/2. 
      goto 1000
3     fnuc8g22=+(+1.+rx)/2.*(+1.+ry)/2.*(+1.-rz)/2. 
      goto 1000
4     fnuc8g22=+(+1.-rx)/2.*(+1.+ry)/2.*(+1.-rz)/2. 
      goto 1000
5     fnuc8g22=+(+1.-rx)/2.*(+1.-ry)/2.*(+1.+rz)/2. 
      goto 1000
6     fnuc8g22=+(+1.+rx)/2.*(+1.-ry)/2.*(+1.+rz)/2. 
      goto 1000
7     fnuc8g22=+(+1.+rx)/2.*(+1.+ry)/2.*(+1.+rz)/2. 
      goto 1000
8     fnuc8g22=+(+1.-rx)/2.*(+1.+ry)/2.*(+1.+rz)/2. 
      goto 1000
1000  return
      end

      subroutine nuc8g23(refc,shpr,rctr,crtr)
      implicit real*8 (a-h,o-z)
      dimension refc(3),shpr(8,4),rctr(3,3),crtr(3,3)
      external fnuc8g23
      rx=refc(1)
      ry=refc(2)
      rz=refc(3)
      call dshap(fnuc8g23,refc,shpr,3,8,1)
      return
      end

      real*8 function fnuc8g23(refc,n)
      implicit real*8 (a-h,o-z)
      common /ccnuc8g2/ xa(8),ya(8),za(8)
      common /vnuc8g2/ rctr(3,3),crtr(3,3)
      dimension refc(3)
      common /coord/ coor(3),coora(27,3)
      x=coor(1)
      y=coor(2)
      z=coor(3)
      rx=refc(1)
      ry=refc(2)
      rz=refc(3)
      goto (1,2,3,4,5,6,7,8) n
1     fnuc8g23=+(+1.-rx)/2.*(+1.-ry)/2.*(+1.-rz)/2. 
      goto 1000
2     fnuc8g23=+(+1.+rx)/2.*(+1.-ry)/2.*(+1.-rz)/2. 
      goto 1000
3     fnuc8g23=+(+1.+rx)/2.*(+1.+ry)/2.*(+1.-rz)/2. 
      goto 1000
4     fnuc8g23=+(+1.-rx)/2.*(+1.+ry)/2.*(+1.-rz)/2. 
      goto 1000
5     fnuc8g23=+(+1.-rx)/2.*(+1.-ry)/2.*(+1.+rz)/2. 
      goto 1000
6     fnuc8g23=+(+1.+rx)/2.*(+1.-ry)/2.*(+1.+rz)/2. 
      goto 1000
7     fnuc8g23=+(+1.+rx)/2.*(+1.+ry)/2.*(+1.+rz)/2. 
      goto 1000
8     fnuc8g23=+(+1.-rx)/2.*(+1.+ry)/2.*(+1.+rz)/2. 
      goto 1000
1000  return
      end

      subroutine tnuc8g2(refc,coor,coorr,rc)
      implicit real*8 (a-h,o-z)
      dimension refc(3),coor(3),coorr(3,8),rc(3,3)
      common /ccnuc8g2/ x(8),y(8),z(8)
      external ftnuc8g2
      do 100 n=1,8
      x(n)=coorr(1,n)
      y(n)=coorr(2,n)
      z(n)=coorr(3,n)
100   continue
      rx=refc(1)
      ry=refc(2)
      rz=refc(3)
      call dcoor(ftnuc8g2,refc,coor,rc,3,3,1)
      return
      end

      real*8 function ftnuc8g2(refc,n)
      implicit real*8 (a-h,o-z)
      dimension refc(3)
      common /ccnuc8g2/ x(8),y(8),z(8)
      common /vnuc8g2/ rctr(3,3),crtr(3,3)
      rx=refc(1)
      ry=refc(2)
      rz=refc(3)
      goto (1,2,3) n
1     ftnuc8g2=+(+(+1.-rx)/2*(+1.-ry)/2*(+1.-rz)/2)*x(1)+(+
     & (+1.+rx)/2*(+1.-ry)/2*(+1.-rz)/2)*x(2)+(+(+1.+rx)/2*
     & (+1.+ry)/2*(+1.-rz)/2)*x(3)+(+(+1.-rx)/2*(+1.+ry)/2*
     & (+1.-rz)/2)*x(4)+(+(+1.-rx)/2*(+1.-ry)/2*(+1.+rz)/2)*x(5)
     & +(+(+1.+rx)/2*(+1.-ry)/2*(+1.+rz)/2)*x(6)+(+(+1.+rx)/
     & 2*(+1.+ry)/2*(+1.+rz)/2)*x(7)+(+(+1.-rx)/2*(+1.+ry)/
     & 2*(+1.+rz)/2)*x(8)
      goto 1000
2     ftnuc8g2=+(+(+1.-rx)/2*(+1.-ry)/2*(+1.-rz)/2)*y(1)+(+
     & (+1.+rx)/2*(+1.-ry)/2*(+1.-rz)/2)*y(2)+(+(+1.+rx)/2*
     & (+1.+ry)/2*(+1.-rz)/2)*y(3)+(+(+1.-rx)/2*(+1.+ry)/2*
     & (+1.-rz)/2)*y(4)+(+(+1.-rx)/2*(+1.-ry)/2*(+1.+rz)/2)*y(5)
     & +(+(+1.+rx)/2*(+1.-ry)/2*(+1.+rz)/2)*y(6)+(+(+1.+rx)/
     & 2*(+1.+ry)/2*(+1.+rz)/2)*y(7)+(+(+1.-rx)/2*(+1.+ry)/
     & 2*(+1.+rz)/2)*y(8)
      goto 1000
3     ftnuc8g2=+(+(+1.-rx)/2*(+1.-ry)/2*(+1.-rz)/2)*z(1)+(+
     & (+1.+rx)/2*(+1.-ry)/2*(+1.-rz)/2)*z(2)+(+(+1.+rx)/2*
     & (+1.+ry)/2*(+1.-rz)/2)*z(3)+(+(+1.-rx)/2*(+1.+ry)/2*
     & (+1.-rz)/2)*z(4)+(+(+1.-rx)/2*(+1.-ry)/2*(+1.+rz)/2)*z(5)
     & +(+(+1.+rx)/2*(+1.-ry)/2*(+1.+rz)/2)*z(6)+(+(+1.+rx)/
     & 2*(+1.+ry)/2*(+1.+rz)/2)*z(7)+(+(+1.-rx)/2*(+1.+ry)/
     & 2*(+1.+rz)/2)*z(8)
      goto 1000
1000  return
      end

