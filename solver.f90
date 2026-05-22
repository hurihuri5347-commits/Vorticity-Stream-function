module solver
    use param
    implicit none
    contains

    subroutine vor(NM,JM,Om,psi,courx,coury,diffx,diffy,u,v,bc_type)
        integer, intent(in) :: NM,JM
        real, intent(inout) :: courx(NM,JM), coury(NM,JM), diffx, diffy
        real, intent(inout) :: Om(NM,JM), psi(NM,JM), u (NM,JM), v(NM,JM)
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
                        Om(1,j) = 2*(psi(2,j)-psi(1,j))/dx**2
                    case (12) ! right wall
                        Om(NM,j) = 2*(psi(NM,j)-psi(NM-1,j))/dx**2
                    case (13) ! bottom wall
                        Om(i,1) = 2*(psi(i,1)-psi(i,2))/dy**2
                    case (14) ! top wall
                        Om(i,JM) = 2*(psi(i,JM)-psi(i,JM-1))/dy**2
                    case (15) ! moving wall
                        Om(i,JM) = (-psi(i,JM)+8*psi(i,JM-1)-7*psi(i,JM-2))/(2*dy**2) - 3*u_inf/dy
                    case (2)  ! inflow
                        Om(1,j) = 2*(psi(1,j)-psi(2,j))/dx**2 - (psi(1,j+1)-2*psi(1,j)+psi(1,j-1))/dy**2
                    case (3)  ! outflow
                        Om(NM,j) = 2*(psi(NM,j)-psi(NM-1,j))/dx**2 - (psi(NM,j+1)-2*psi(NM,j)+psi(NM,j-1))/dy**2
                    case(4) !far field
                        Om(i,j) = 0.0
                        psi(i,j) = 0.0
                    case (-1) ! solid region (inactive cell)
                        Om(i,j) = 0.0
                        psi(i,j) = 0.0

                end select
            end do
        end do

        diffx = 1/Re*(dt/dx**2)
        diffy = 1/Re*(dt/dy**2)
        do i = 1,NM
            do j = 1,JM
                courx(i,j) = u(i,j)*dt/(dx)
                coury(i,j) = v(i,j)*dt/(dy)
            end do
        end do


        ! =======================================================
        !                       X-sweep
        ! =======================================================

        do j = 2,JM-1
            do i = 3,NM-1
                ax(i-2) = -0.5*(0.5*courx(i-1,j)+diffx)
            end do
            bx = 1.0 + diffx
            do i = 2,NM-2
                cx(i-1) = -0.5*(-0.5*courx(i+1,j)+diffx)
            end do

            do i = 2,NM-1
                RHSx(i-1) = 0.5*(0.5*coury(i,j-1)+diffy)*Om(i,j-1) + (1.0-diffy)*Om(i,j) + 0.5*(-0.5*coury(i,j+1)+diffy)*Om(i,j+1)
            end do

            RHSx(1) = RHSx(1) - ax(1)*Om(1,j)
            RHSx(NM-2) = RHSx(NM-2) - cx(NM-2)*Om(NM,j)

            call Tri(ax,bx,cx,RHSx)
            do i = 2,NM-1
                Om_tmp(i,j) = RHSx(i-1)
            end do
        end do

        ! =======================================================
        !                       Y-sweep
        ! =======================================================
        do i = 2,NM-1
            do j = 3,JM-1
                ay(j-2) = -0.5*(0.5*coury(i,j-1)+diffy)
            end do
            by = 1.0 + diffy
            do j = 2,JM-2
                cy(j-1) = 0.5*(0.5*coury(i,j+1)-diffy)
            end do
            do j = 2,JM-1
                RHSy(j-1) = 0.5*(0.5*courx(i-1,j)+diffx)*Om_tmp(i-1,j) + (1.0-diffx)*Om_tmp(i,j) + 0.5*(-0.5*courx(i+1,j)+diffx)*Om_tmp(i+1,j)
            end do
            RHSy(1) = RHSy(1) - ay(1)*Om_tmp(i,1)
            RHSy(JM-2) = RHSy(JM-2) - cy(JM-2)*Om_tmp(i,JM)

            call Tri(ay,by,cy,RHSy)
            do j = 2,JM-1
                Om(i,j) = RHSy(j-1)
            end do
        end do


    end subroutine vor


    subroutine poisson(NM,JM,psi,Om, u, v, dx, dy,iter,tol)
        integer, intent(in) :: NM,JM
        real, intent(in) :: Om(NM,JM), dx, dy
        real, intent(inout) :: psi(NM,JM), u(NM,JM), v(NM,JM)
        integer, intent(inout) :: iter
        real, intent(inout) :: tol

        integer :: i,j
        real :: psi_tmp(NM,JM), beta
        integer :: max_iter
        max_iter = 10000

        beta = dx/dy

        tol = 1.0
        iter = 1
        
        do while(tol > 1.0e-4)
            iter = iter + 1
            if(iter > max_iter) then
                print *, "Warning: Poisson not converged at iter=", iter
                exit
            end if

            psi_tmp = psi

            do j = 2, JM-1
                do i = 2, NM-1
                    psi_tmp(i,j) = 1/(2.0*(1.0+beta**2))*(psi(i+1,j)+psi(i-1,j)+beta**2*(psi(i,j+1)+psi(i,j-1))+dx**2*Om(i,j))
                end do
            end do
            tol = maxval(abs(psi-psi_tmp))
            psi = psi_tmp
        end do
        
        do j = 2, JM-1
            do i = 2, NM-1
                u(i,j) =  (psi(i,j+1) - psi(i,j-1)) / (2.0*dy)
                v(i,j) = -(psi(i+1,j) - psi(i-1,j)) / (2.0*dx)
            end do
        end do
        
    end subroutine poisson



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

