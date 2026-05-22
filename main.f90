program main
    use param
    use solver
    use postprocess
    use grid
    use init
    implicit none

    integer :: NM, JM, KM, k, plot_count
    real, allocatable :: x(:), y(:), Om(:,:), psi(:,:), u(:,:), v(:,:)
    real, allocatable :: cx(:,:), cy(:,:)
    real :: diffx, diffy,tol
    integer, allocatable :: bc_type(:,:)
    character(len=64) :: filename
    integer :: iter

    NM = int(L/dx)+1; JM = int(H/dy)+1
    allocate(x(NM), y(JM))
    allocate(Om(NM, JM), psi(NM, JM), u(NM, JM), v(NM, JM))
    allocate(cx(NM, JM), cy(NM, JM))
    allocate(bc_type(NM, JM))
    
    call gen_grid(dx,dy,x,y,L,H,NM,JM)

    call make_bc(NM,JM, bc_type)
    call init_flow(NM,JM,psi,Om,u,v)

    KM = int(t_end/dt)+1

    plot_count = 0

    do k = 1, KM

        call poisson(NM,JM,psi,Om,u,v,dx,dy,iter,tol)
        call vor(NM,JM,Om,psi,cx,cy,diffx,diffy,u,v,bc_type)

        if(mod(k, int(t_plot/dt)) == 0) then
            plot_count = plot_count + 1
            write(filename, '(A,I4.4,A)') 'output/output_', plot_count, '.plt'
            call save_tec(trim(filename), NM, JM, dx, dy, u, v, Om, psi, k*dt)
            print *, 'poisson iteration: ', iter, ' with tol: ', tol
        end if

    end do



end program main