program mainprogram
    implicit none
    
    real A(100,100)
    real b(100)
    
    integer n, asize, g

    g = 1
    n = 11
    asize = 100

    write(*,*) "asdasdasdasd"

    call setupvector(b,asize,g)

    write(*,*) "asd"

end program mainprogram

subroutine setupvector(b, bsize, g)
    real dx 
    integer bsize, g, x,y, lim, i, j, k
    real b(1)
        k = (n-2)*(n-1);

        lim = (n-1)**2

        write(*,*) "testetesteetest"        

        do i=1,lim
            b(i) = g
        end do

        write(*,*) "Noe rart"    

        lim = (n-1)
        do i=1,lim
            b(i) = b(i) - (1/4)*(i**2)
            b(i+k) = b(i+k) - (1/4)*((i**2)+(n**2))
        end do 

        lim = lim -1
        do i=1,lim
            b((i-1)*lim+1)    = b((i-1)*lim+1) - (1/4)*(i**2)
            b(i*lim)          = b(i*lim) - (1/4)*((i**2)+(n**2)) 
        end do 
        
end 
