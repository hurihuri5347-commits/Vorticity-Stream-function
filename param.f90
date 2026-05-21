module param
    implicit none

    real, parameter :: sos  = 1.0 , Re = 100.0, rho = 1.225, nu = 0.0025 &
                       ,L = 1.0, H = 1.0, dx = 0.1, dy = 0.1, dt = 0.01
    real, parameter :: u_inf = Re*nu/L
    integer, parameter :: eps_x = 1, eps_y = 1  ! 1: first-order upwind, 0: second-order central

end module param