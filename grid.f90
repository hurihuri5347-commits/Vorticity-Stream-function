module grid
    implicit none
    contains

    subroutine gen_grid(dx,dy,x,y,L,H,NM,JM)
        integer, intent(in) :: NM,JM
        real, intent(inout) :: x(NM), y(JM)
        real, intent(in) :: L, H, dx, dy
        integer :: i

        do i = 1, NM
            x(i) = (i-1)*dx
        end do
        do i = 1, JM
            y(i) = (i-1)*dy
        end do

    end subroutine gen_grid

end module grid