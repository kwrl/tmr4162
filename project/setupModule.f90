module setupModule
    implicit none
      
    contains
 
    function setupMatrix(n)
        integer::n
        real, dimension(n,n)::setupMatrix
                     
    end function setupMatrix  

    function setupVector(n,g)
        integer::n
        integer::g
        real, dimension((n-1)**2)::setupVector 
        integer x,y, lim, i, j, k
        real dx 


        k = (n-2)*(n-1);

        lim = (n-1)**2
        do i=1,lim
            setupVector(i) = g
        end do    

        lim = (n-1)
        do i=1,lim
            setupVector(i) = setupVector(i) - (1/4)*(i**2)
            setupVector(i+k) = setupVector(i+k) - (1/4)*((i**2)+(n**2))
        end do 

        lim = lim -1
        do i=1,lim
            setupVector((i-1)*lim+1)    = setupVector((i-1)*lim+1) - (1/4)*(i**2)
            setupVector(i*lim)          = setupVector(i*lim) - (1/4)*((i**2)+(n**2)) 
        end do 
        
    end function setupVector 

end module setupModule
