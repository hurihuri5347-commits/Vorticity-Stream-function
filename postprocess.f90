module postprocess
    implicit none
    contains
    subroutine save_vtk(filename, NM, JM, dx, dy, Q)
        implicit none
        
        ! --- 인자 선언 ---
        character(len=*), intent(in) :: filename
        integer, intent(in)          :: NM, JM
        real, intent(in)             :: dx, dy
        real, intent(in)             :: Q(3, 0:NM+1, 0:JM+1)
        
        ! --- 내부 변수 선언 ---
        integer :: i, j, unit_num
        integer :: total_points
        real    :: x_coord, y_coord
        
        unit_num = 20  ! 파일 파일 번호 (유연하게 변경 가능)
        
        ! 전체 격자 점의 개수 (내부 격자 1~NM, 1~JM 기준)
        total_points = NM * JM
        
        ! 파일 열기
        open(unit=unit_num, file=filename, status='unknown', action='write')
        
        ! ==========================================
        ! 1. VTK 헤더 작성 (Legacy FORMAT)
        ! ==========================================
        write(unit_num, '(A)') '# vtk DataFile Version 3.0'
        write(unit_num, '(A)') 'Step Flow CFD Results'
        write(unit_num, '(A)') 'ASCII'
        write(unit_num, '(A)') 'DATASET STRUCTURED_GRID'
        
        ! 격자 크기 지정 (X방향 점 개수, Y방향 점 개수, Z방향은 2D이므로 1)
        write(unit_num, '(A, I6, A, I6, A)') 'DIMENSIONS ', NM, ' ', JM, ' 1'
        
        ! ==========================================
        ! 2. 격자 기하학적 좌표 (POINTS) 작성
        ! ==========================================
        write(unit_num, '(A, I8, A)') 'POINTS ', total_points, ' float'
        
        ! Fortran의 Column-major 순서와 VTK의 데이터 나열 순서를 맞추기 위해
        ! Outer loop: j (Y축), Inner loop: i (X축) 순으로 출력합니다.
        do j = 1, JM
            do i = 1, NM
                ! 물리적 좌표 계산 (물리 도메인이 (0,0)에서 시작한다고 가정)
                x_coord = real(i-1) * dx
                y_coord = real(j-1) * dy
                
                ! VTK는 기본적으로 3차원 좌표를 요구하므로 Z좌표는 0.0 고정
                write(unit_num, '(3F12.5)') x_coord, y_coord, 0.0e0
            end do
        end do
        
        ! ==========================================
        ! 3. 물리 데이터 (POINT_DATA) 작성
        ! ==========================================
        write(unit_num, '(A, I8)') 'POINT_DATA ', total_points
        
        ! --- [데이터 1] 압력 (Pressure, Scalar 데이터) ---
        write(unit_num, '(A)') 'SCALARS Pressure float 1'
        write(unit_num, '(A)') 'LOOKUP_TABLE default'
        do j = 1, JM
            do i = 1, NM
                ! Q(1, i, j) 에 압력이 저장되어 있다고 가정
                write(unit_num, '(F12.5)') Q(1, i, j)
            end do
        end do
        
        ! --- [데이터 2] 속도 벡터 (Velocity Vector 데이터) ---
        ! VTK에서 벡터 플롯(화살표)을 보려면 3성분(u, v, w)이 세트여야 합니다.
        write(unit_num, '(A)') 'VECTORS Velocity float'
        do j = 1, JM
            do i = 1, NM
                ! Q(2,:,:)는 u 속도, Q(3,:,:)는 v 속도, w 속도는 0.0 고정
                write(unit_num, '(3F12.5)') Q(2, i, j), Q(3, i, j), 0.0e0
            end do
        end do
        
        ! 파일 닫기
        close(unit_num)
        
        print *, "Successfully saved VTK file: ", filename
        
    end subroutine save_vtk

    subroutine save_tecplot(filename, NM, JM, dx, dy, Q)
        implicit none
 
        ! --- 인자 선언 ---
        character(len=*), intent(in) :: filename
        integer, intent(in)          :: NM, JM
        real, intent(in)             :: dx, dy
        real, intent(in)             :: Q(3, 0:NM+1, 0:JM+1)
 
        ! --- 내부 변수 선언 ---
        integer :: i, j, unit_num
        real    :: x_coord, y_coord, umag
 
        unit_num = 21
 
        open(unit=unit_num, file=filename, status='unknown', action='write')
 
        ! ==========================================
        ! 1. Tecplot ASCII 헤더
        ! ==========================================
        write(unit_num, '(A)') 'TITLE = "Step Flow CFD Results"'
        write(unit_num, '(A)') 'VARIABLES = "X" "Y" "P" "U" "V" "Umag"'
 
        ! ZONE: 구조격자(IJK-ordered) 형식
        write(unit_num, '(A, I6, A, I6, A)') &
            'ZONE T="Flow Field", I=', NM, ', J=', JM, ', F=POINT'
 
        ! ==========================================
        ! 2. 데이터 출력 (X Y P U V)
        !    Tecplot POINT 형식: 점 하나당 한 줄
        !    루프 순서: i(X) 먼저, j(Y) 바깥 — Tecplot IJ-ordering 기준
        ! ==========================================
        do j = 1, JM
            do i = 1, NM
                x_coord = real(i-1) * dx
                y_coord = real(j-1) * dy
                umag = sqrt(Q(2,i,j)**2 + Q(3,i,j)**2)
                write(unit_num, '(6E18.9)') &
                    x_coord, y_coord, Q(1,i,j), Q(2,i,j), Q(3,i,j), umag
            end do
        end do
 
        close(unit_num)
 
        print *, "Successfully saved Tecplot file: ", filename
 
    end subroutine save_tecplot

end module postprocess