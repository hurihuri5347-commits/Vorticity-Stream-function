program main
    use param
    use solver
    use postprocess
    use grid
    use init
    implicit none

    integer :: NM, JM
    real, allocatable :: x(:), y(:), Om(:,:), phi(:,:)
    real, allocatable :: cx(:,:), cy(:,:), diffx, diffy
    integer, allocatable :: bc_type(:,:)

    NM = int(L/dx)+1; JM = int(H/dy)+1
    allocate(x(NM), y(JM))
    allocate(Om(NM, JM), phi(NM, JM))
    allocate(cx(NM, JM), cy(NM, JM))
    allocate(bc_type(NM, JM))
    
    call make_grid(NM, JM, dx, dy, x, y)

    

    

    





end program main