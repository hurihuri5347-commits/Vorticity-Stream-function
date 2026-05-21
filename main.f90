program main
    use param
    use solver
    use postprocess
    use grid
    implicit none

    integer :: NM, JM
    real :: L, H, dx, dy
    real, allocatable :: x(:), y(:)

    allocate(x(0:NM+1), y(0:JM+1))

    ! Set grid parameters
    L = 1.0; H = 1.0; dx = 0.1; dy = 0.1
    NM = int(L/dx)+1; JM = int(H/dy)+1

    

    





end program main