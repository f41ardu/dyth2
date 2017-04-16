        function gtheta(h,k,l,lambda,imat)
c
c --- bragg winkel
c
        implicit none
        integer imat
        real h,k,l
        real hkl,a,lambda,gtheta

        if(imat .eq. 1) a = 5.430102018e0 ! Gi_kn SI
        if(imat .eq. 2) a = 3.567
        if(imat .eq. 3) a = 5.658
        hkl = h*h + k*k + l*l
        hkl = sqrt(hkl)
        gtheta = lambda*hkl/(2.e0*a)
        gtheta = asin(gtheta)
        return
        end
c
