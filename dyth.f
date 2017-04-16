      program two_beam
c
c      
c --- Berechnung der Reflektionskurven (Intrinsikurven)
c     im Zweistrahlfall nach den im Zachariasen  
c     angegebenen Gleichungen (modifiziert) 
c     
c     (C) 1993,94,95,96,97,98,2017 Th. Rautenstrauch All Rights Reserved 
c
c     Fortran V1.0 31/01.96
c     g77 / OpenBSD
c
      implicit none
c      
      complex  chi0,chih,q,z,D,z1,z2,x1,x2,d01,d02
      complex  c1,c2,phi1,phi2
      complex DH,D0,DHH,D00
      complex ca,cb,cc
      complex a(3),roots(2)
      complex swap
      complex yyy
      complex phid1,phid2,dd1,dd2
      complex test7,test8,phs1,phs2,cfakt
      real test1,test2,test3,test4,test5,test6
      real IB0,IBH,IB00,IBHH
      real lambda,lam2,pol,rho 
      real gamma0,gammah,b,alpha,theta,dp,k0,t1,dpp,asy,y
      real q1,q2
      real pi
      real gtheta ,delta_theta
      real h,k,l
      real r1,r2,phaseB,phaseL
      real xstart,xend,xstep
      real mue
      real rh_store,rl_store,rh_temp
c      
      integer ichoice,imat,ndata,nhphase,nlphase,ncount
      logical laue,bragg,par,sigma
      open(77,file='2.dat')
c
107   format(5f16.8)
c      
      pi = 4.e0*atan(1.e0)  
      nhphase=1
      nlphase=1
c      
      write(6,*) 'lambda :'
      read(5,*) lambda
      lam2 = lambda
      lambda = lambda * 1.e-10 ! Angstroem -> m
      write(6,*) 'h,k,l :'
      read(5,*) h,k,l
      write(6,*) 'Material : Si 1 / C 2 /Ge 3' 
      read(5,*) imat
c
      call strfac(real(h),real(k),real(l),real(lam2),chih,imat)
      call strfac(0.e0,0.e0,0.e0,real(lam2),chi0,imat)
c
      mue = -2.e0*pi*aimag(chi0)/(lambda)  
c      
      write(6,*) 'chi0 :',chi0
      write(6,*) 'chih :',chih
c
      write(6,*) ' Dicke in mm :'
      read(5,*) t1
      t1 = t1 * 1.e-3 !mm -> m
      write(6,*) 'mue*t :',mue*t1 ! so what m
      write(6,*) '1/mue', 1/mue 
      write(6,*) 'I/I0 :',exp(-mue*t1)
c
      laue=.false.
      bragg=.false.
123   write(6,*) 'Laue oder Bragg (1/2) '
      read(5,*) ichoice
      if(ichoice.eq.1) laue=.true.
      if(ichoice.eq.2) bragg=.true.
      if(ichoice.ne.1.and.ichoice.ne.2)goto 123
c
      par=.false.
      sigma=.false.
124   write(6,*) 'Pi oder Sigma Polarisation (1/2) '
      read(5,*) ichoice
      if(ichoice.eq.1) par=.true.
      if(ichoice.eq.2) sigma=.true.
      if(ichoice.ne.1.and.ichoice.ne.2)goto 124
c

      k0 = 1.d0/(lambda)
      write(6,*) 'k0 :',k0
c      
      theta = gtheta(h,k,l,lam2,imat)
c      
      write(6,*) 'Theta Bragg : ',theta*180.d0/pi
c        
      write(6,*) ' asy :'      
      read(5,*) asy
      asy =  asy*pi/180.e0
c
c Laue 
      if(laue) then
         gamma0 = cos(theta - asy)
         gammah = cos(theta + asy)
      endif
c Bragg
      if(bragg) then
         gamma0 =  sin(theta + asy)
         gammah = -sin(theta - asy)
      
      endif
c
      write(6,*) gamma0,gammah
c     
      b = gamma0/gammah
c     
      if(par) then
        pol = cos(2.e0*theta) !paralell Pol
      endif
c     
      if(sigma) then
         pol = 1.e0     
      endif
c      
      write(6,*) 'Polarisation :',pol
      write(6,*) 'b :',b
c     
      write(6,*) 'Dynamische Breite :'
      delta_THETA = 2.*abs(pol)*cabs(chih)/
     .            (sin(2.*theta)*
     .            sqrt(abs(gamma0/gammah)))
      
      write(6,*) delta_theta*(180./pi)*3600.
      write(6,*) "Anzahl der Datenpunkte ?"
      read(5,*) ndata
      xstart = -23.47724114/3600.*pi/180. 
      xend   = -xstart
      xstep = (xend-xstart)/ndata
      do 10 dp = xstart,xend,xstep
         ncount = ncount + 1
c         
         dpp = -dp*(180.e0/pi)!*3600.e0
c         
         alpha = 2.d0*dp*sin(2.d0*theta)
c         
         z = chi0*(1 - b)/2. + b/2*alpha
c
	 q = b*chih**2
c
         y = -real(z)/(sqrt(abs(b))*real(chih))
         yyy = -z/(sqrt(abs(b))*chih)  
c
         x1 = ( -z + csqrt( q + z**2))/chih
	 x2 = ( -z - csqrt( q + z**2))/chih
c
	 d01 = 0.5*(chi0 - z + csqrt( q + z**2 ))
	 d02 = 0.5*(chi0 - z - csqrt( q + z**2 ))
c
         q1 = (cabs(x1)**2 -1.)/(cabs(x1)**2+1)
         q2 = (cabs(x2)**2 -1.)/(cabs(x2)**2+1)
c         
         phi1 = 2.0*pi*k0*d01/gamma0 
         phi2 = 2.0*pi*k0*d02/gamma0 
c         
c         call me_cexp(c1,(0.e0,-1.e0)*phi1*t1)
c         call me_cexp(c2,(0.e0,-1.e0)*phi2*t1)
         c1=cexp((0.e0,-1.e0)*phi1*t1)
	 c2=cexp((0.e0,-1.e0)*phi2*t1)
c
         if(laue) then
            call laueit(x1,x2,c1,c2,d0,dh)
         endif
c        
         if(bragg) then
             call braggit(x1,x2,c1,c2,d0,dh)
         endif
c         
         IBH = CABS(DH)**2
         IB0 = CABS(D0)**2
c
         write(77,107) real(dpp + theta*180./pi),y,dpp*3600, 
     .   real(IBH/abs(b)),real(IB0/abs(b))
c         
10       continue
         stop
         end
c
        subroutine me_cexp(result,arg)
c
c--- Zerlegung der complexen Exponentialfunktion
c    Auswertung der trignometischen Funktionen ueber 
c    Hauptwerte  
c
        complex result,arg
        real rarg,iarg,pi
        real resultrarg,resultiarg
        real emuet
c
        pi = 4.e0*atan(1.e0)
c
        result = (0.e0,0.e0)
c        
        rarg = real(arg)
        iarg = aimag(arg)
c
c        iarg = mod(iarg,2.*pi)
c        
        if(rarg.ge.0.e0) then
           if(rarg.gt.38.0) then 
             emuet = exp(38.0)
           else
             emuet=exp(rarg)
           endif
        endif
c        
        if(rarg.lt.0.e0) then
           if(rarg.le.-38.0) then
              emuet = exp(-38.0)
           else 
              emuet=exp(rarg)
           endif
        endif
c        
        resultrarg=cos(iarg)
        resultiarg=sin(iarg)
c
        result = emuet*cmplx(resultrarg,resultiarg)
        return
c        
        end
c
