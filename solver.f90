module solver
    use param
    implicit none
    contains

    subroutine vor(NM,JM,Om,phi,courx,coury,diffx,diffy,u,v,bc_type)
        integer, intent(in) :: NM,JM
        real, intent(inout) :: courx(NM,JM), coury(NM,JM), diffx, diffy
        real, intent(inout) :: Om(NM,JM), phi(NM,JM), u (NM,JM), v(NM,JM)
        integer, intent(in) :: bc_type(NM,JM)

        integer :: i,j
        real :: Om_tmp(NM,JM), ax(NM-3), bx(NM-2), cx(NM-2), RHSx(NM-2)
        real :: ay(JM-3), by(JM-2), cy(JM-2), RHSy(JM-2)


        !========================================================
        !                     경계조건 설정
        !========================================================
        do j = 1, JM
            do i = 1, NM
                select case (bc_type(i,j))
                    case (0)  ! internal flow

                    case (11) ! left wall
                        
                    case (12) ! right wall
                        
                    case (13) ! bottom wall
                        
                    case (14) ! right wall
                    
                    case (15) ! inside wall
                        

                    case (2)  ! inflow
                        
                    case (3)  ! outflow
                    

                end select
            end do
        end do


        ! =======================================================
        !                       X-sweep
        ! =======================================================
        diffx = 1/Re*(dt/dx**2)
        diffy = 1/Re*(dt/dy**2)
        do i = 1,NM
            do j = 1,JM
                courx(i,j) = u(i,j)*dt/(dx)
                coury(i,j) = v(i,j)*dt/(dy)
            end do
        end do

        do i = 2,NM-1
            ax(i-1) = -0.5*(0.5*courx(i,j)+diffx)
        end do
        do i = 2,NM-1
            bx(i) = 1.0 + 2.0*diffx
        end do
        do i = 1,NM-2
            cx(i) = -0.5*(-0.5*courx(i,j)+diffx)
        end do


    end subroutine vor



    subroutine Tri(a,b,c,RHS)
        real, intent(inout) :: a(:),b(:),c(:)
        real, intent(inout) :: RHS(:)

        integer :: i,n

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

