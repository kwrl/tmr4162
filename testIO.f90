program testIO
    use iomodule
    implicit none
    
    character(LEN=5) filepath
    real, pointer, dimension(:,:)::matrix

    filepath='input'

    write(*,*) "filepath: ", filepath
    matrix = readMatrix(filepath,5)

    write(*,*) "matrix(1,1)", matrix(1,1)

end program testIO
