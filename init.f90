module init
    use param
    implicit none
    contains

    subroutine make_bc(NM,JM, bc_type)
        integer, intent(in) :: NM, JM
        real, intent(inout) :: bc_type(NM,JM)

        integer :: i,j

        do j = 1, JM
            do i = 1, NM

                if(j==JM) then
                    bc_type(i,j) = 2 ! inflow
                else if(i==1 .and. j < JM) then
                    bc_type(i,j) = 11 ! left wall
                else if(j == 1) then
                    bc_type(i,j) = 13 ! bottom wall
                ! else if(i==1 .and. j < 40 .or. i==1 .and. j>60 .and. j<JM) then
                !     bc_type(i,j) = 12 ! left wall
                ! else if(i==1 .and. j >= 40 .and. j <= 60) then
                !     bc_type(i,j) = 2 ! inflow
                ! else if(i==NM .and. j >= 40 .and. j <= 60) then
                !     bc_type(i,j) = 3 ! outflow
                else if(i==NM .and. j < JM) then
                    bc_type(i,j) = 11 ! right wall

                else
                    bc_type(i,j) = 0 ! internal flow
                end if
            end do
        end do

    end subroutine make_bc



end module init