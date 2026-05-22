module init
    use param
    implicit none
    contains

    subroutine make_bc(NM,JM, bc_type)
        integer, intent(in) :: NM, JM
        integer, intent(inout) :: bc_type(NM,JM)

        integer :: i,j, ISTEP, JSTEP

        ISTEP = int(NM/5)
        JSTEP = int(JM/2)

        do j = 1, JM
    do i = 1, NM

        !====================================================
        ! Top wall
        !====================================================
        if(j == JM) then

            bc_type(i,j) = 4      ! far field (top wall)

        !====================================================
        ! Inflow (left side above step)
        !====================================================
        else if(i == 1 .and. j > JSTEP) then

            bc_type(i,j) = 2       ! inflow

        !====================================================
        ! Step vertical wall
        !====================================================
        else if(i == ISTEP .and. j <= JSTEP) then

            bc_type(i,j) = 11      ! wall

        !====================================================
        ! Bottom wall downstream of step
        !====================================================
        else if(j == 1 .and. i >= ISTEP) then

            bc_type(i,j) = 13      ! bottom wall

        !====================================================
        ! Bottom blocked region (inside step)
        !====================================================
        else if(i < ISTEP .and. j <= JSTEP) then

            bc_type(i,j) = -1      ! solid region (inactive cell)

        !====================================================
        ! Outflow
        !====================================================
        else if(i == NM) then

            bc_type(i,j) = 3       ! outflow

        !====================================================
        ! Internal fluid
        !====================================================
        else

            bc_type(i,j) = 0

        end if

    end do
end do

    end subroutine make_bc

    subroutine init_flow(NM,JM,psi,Om,u,v)
        integer, intent(in) :: NM, JM
        real, intent(inout) :: psi(NM,JM), Om(NM,JM)
        real, intent(inout) :: u(NM,JM), v(NM,JM)

        integer :: i,j, ISTEP, JSTEP

        ISTEP = int(NM/5)
        JSTEP = int(JM/2)

        do j = 1, JM
            do i = 1, NM

                if(i == 1 .and. j > JSTEP) then
                    u(i,j) = u_inf
                else
                    u(i,j) = 0.0
                end if

                if(j == 1 .and. i >= ISTEP) then
                    v(i,j) = 0.0
                    psi(i,j) = 10.0
                    Om(i,j) = 0.0
                end if

            end do
        end do

    end subroutine init_flow



end module init