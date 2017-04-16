        subroutine braggit(x1,x2,c1,c2,a0,ah)
c
c --- bragg case
c       
        complex x1,x2,c1,c2,a0,ah,b1,b2
c
	b1 = c2*x2/(c2*x2-c1*x1)
c
	b2 = -c1*x1/(c2*x2 - c1*x1)
c
        a0 = c1*b1 + c2*b2
c        
        ah = x1*b1 + x2*b2 
c        
        return
        end
c
        subroutine laueit(x1,x2,c1,c2,a0,ah)
c
c --- laue case
c       
        complex x1,x2,c1,c2,a0,ah,b1,b2
c
	b1 =  x2/(x2 - x1)
c
	b2 = -x1/(x2 - x1)
c
        a0 = c1*b1 + c2*b2
c        
        ah = c1*x1*b1 + c2*x2*b2 
c        
        return
        end
