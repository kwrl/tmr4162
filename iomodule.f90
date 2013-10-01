module iomodule
    implicit none
    
    contains
    
    function readMatrix(filepath, filepath_length)
        
        integer::filepath_length
        character(LEN=filepath_length)::filepath
        real, pointer, dimension(:,:)::readMatrix
        integer fi,i,j,m,n

        fi = 10

        open(fi, FILE=filepath, status="old") 
        
        read(10,*) m, n

        allocate(readMatrix(m,n))

        do i = 1, m
            read(10,*) readMatrix(i,1:n)
        end do

    end function readMatrix


end module iomodule
