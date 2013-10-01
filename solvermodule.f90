module solvermodule
    implicit none
    
    contains

    function jacobi(A,b,x,n)
        use vectorsubs
        integer::n
        real, dimension(n)::x
        real, dimension(n)::y
        real, dimension(n)::b
        real, dimension(n)::jacobi
        real, dimension(n,n)::A
        
        real delta, eps, diag, sum
        integer i,j,k,kmax

        delta   = 10**(-10)
        eps     = 0.5*(10**(-4))
        kmax    = 100
         
        do k = 1, kmax
            y(1:n) = x(1:n)
            do i = 1, n
                sum = b(i)
                diag = A(i,i)
                if(abs(diag).LT.delta) then
                    jacobi = x
                    return
                endif
                do j = 1, n
                    if(j.NE.i) then
                        sum = sum-A(i,j)*y(j)
                    endif
                end do
                x(i) = sum/diag
            end do
            if(vectorLength(vectorSubtraction(x,y,n),n).LT.eps) then
                jacobi = x
                return
            endif
        end do

    end function jacobi 

end module solvermodule
