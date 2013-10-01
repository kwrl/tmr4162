program testJacobi
    use solvermodule
    implicit none

    integer i
    real A(3,3), b(3), x(3)    

    A = reshape((/ 2, -1, 0, -1, 3, -1, 0, -1, 2 /)), shape(A)

    b(1) = 1
    b(2) = 8
    b(3) = -5

    x = jacobi(A,b,x,3)

    do i = 1, 3
        write(*,*) "x ", x(i)
    end do 

end program testJacobi

