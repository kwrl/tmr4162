module vectorsubs
    implicit none

    contains

    function vectorSubtraction(x,y,n)

        integer::n
        real, dimension(n)::x
        real, dimension(n)::y
        real, dimension(n)::vectorSubtraction
        integer i

        do i = 1, n
            vectorSubtraction(i) = x(i)-y(i)
            continue
        end do
    end function vectorSubtraction

    function vectorAddition(x,y,n)
        
        integer::n
        real, dimension(n)::x
        real, dimension(n)::y
        real, dimension(n)::vectorAddition
        integer i

        do i = 1, n
            vectorAddition(i) = x(i)+y(i)
            continue
        end do
    end function vectorAddition

    function vectorLength(x,n)
        
        integer::n
        real, dimension(n)::x
        real::vectorLength
        real temp_sum
        integer i

        do i = 1, n
            temp_sum = temp_sum + x(i)**2
        end do
        
        vectorLength = sqrt(temp_sum)

    end function vectorLength
        
end module vectorsubs
