module solver
    use param
    implicit none
    contains

    subroutine Tri(a,b,c,RHS)
        real, intent(inout) :: a(:),b(:),c(:)
        real, intent(inout) :: RHS(:)

        integer :: i,j,n

        n = size(b)
        
        ! 전진소거
        do i = 1,n-1
            b(i+1) = b(i+1)-a(i)*c(i)/b(i)
            RHS(i+1) = RHS(i+1)-a(i)*RHS(i)/b(i)
        end do

        ! 후진대입
        RHS(n) = RHS(n)/b(n)
        do i = n-1,1,-1
            RHS(i) = (RHS(i)-c(i)*RHS(i+1))/b(i)
        end do
        
    end subroutine Tri

    

end module solver

