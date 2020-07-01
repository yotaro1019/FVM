module field_cube
    use params
    implicit none
    type cube_param
        integer :: nx, ny, nz
        real(8) :: dx, dy, dz
        real(8), dimension(:,:,:,:), allocatable :: x !pos CV_cnt(:,:,:,1)=pos_x, 2=pos_y, 3=pos_z
        real(8), dimension(:,:,:,:), allocatable :: u !velocity CV_cnt(:,:,:,1)=vel_x, 2=vel_y, 3=vel_z
        real(8), dimension(:,:,:), allocatable :: p, phi   !pressurer

        contains
        procedure:: field_init => field_initialize
        procedure:: out_cnt_g => output_cnt_g
        procedure:: out_cnt_q => output_cnt_q
    end type cube_param

    type(cube_param) :: cube

contains
    subroutine field_initialize(cube)
        implicit none
        class(cube_param) :: cube
        integer :: i,j,k

        cube%nx = param%nx
        cube%ny = param%ny 
        cube%nz = param%nz 
        cube%dx = param%dx 
        cube%dy = param%dy 
        cube%dz = param%dz

        write(*,*) cube%nx, cube%ny, cube%nz
        write(*,*) cube%dx, cube%dy, cube%dz

        allocate(cube%x(cube%nx, cube%ny, cube%nz, 3))

        
        do i = 1, cube%nx
            cube%x(i,:,:,1) = cube%dx*(i-1)
        end do
        do i = 1, cube%ny
            cube%x(:,i,:,2) = cube%dy*(i-1)
        end do
        do i = 1, cube%nz
            cube%x(:,:,i,3) = cube%dz*(i-1)
        end do
       
        allocate(cube%u(cube%nx, cube%ny, cube%nz, 3))


        allocate(cube%p(cube%nx, cube%ny, cube%nz))
        allocate(cube%phi(cube%nx, cube%ny, cube%nz))
        do i = 1, cube%nx
            do j = 1, cube%ny
                do k = 1, cube%nz
                    cube%p(i,j,k)= 10.0d0
                    cube%u(i,j,k,1)= 20.0d0
                    cube%u(i,j,k,2)= 30.0d0
                    cube%u(i,j,k,3)= 40.0d0
                    cube%phi(i,j,k)= 50.0d0
                end do
            end do
        end do
        return
    end subroutine field_initialize


    !subroutine output_g(cube)
    !    implicit none
    !    class(cube_param) :: cube
    !    integer :: i, j, k
!
    !    write(*,*) "output cube.g"
    !    open(10, file = "cube.g")
!
!
    !    write(10,*) cube%nx, cube%ny, cube%nz
    !    write (10,*) &
    !        ((( cube%x(i,j,k), i = 1, cube%nx ), j = 1, cube%ny ), k = 1, cube%nz ), &
    !        ((( cube%y(i,j,k), i = 1, cube%nx ), j = 1, cube%ny ), k = 1, cube%nz ), &
    !        ((( cube%z(i,j,k), i = 1, cube%nx ), j = 1, cube%ny ), k = 1, cube%nz )
!
    !    close(10)
!
!
    !    return
    !end subroutine output_g

    subroutine output_cnt_g(cube)
        implicit none
        class(cube_param) :: cube
        integer :: i, j, k

        write(*,*) "output_cnt.g"
        open(10, file = "output_cnt.g")


        write(10,*) cube%nx, cube%ny, cube%nz
        write (10,*) &
            ((( cube%x(i,j,k,1), i = 1, cube%nx ), j = 1, cube%ny ), k = 1, cube%nz ), &
            ((( cube%x(i,j,k,2), i = 1, cube%nx ), j = 1, cube%ny ), k = 1, cube%nz ), &
            ((( cube%x(i,j,k,3), i = 1, cube%nx ), j = 1, cube%ny ), k = 1, cube%nz )

        close(10)

        return
    end subroutine output_cnt_g

    subroutine output_cnt_q(cube)
        implicit none
        class(cube_param) :: cube
        integer :: i,j,k

            open(10, file = "output_cnt.q")    
            
            write(10,*) cube%nx, cube%ny, cube%nz
            write(10,*) 1.0d0, 1.0d0, 1.0d0, 1.0d0
            write(10,*) ((( cube%p(i,j,k), i = 1, cube%nx ), j = 1, cube%ny ), k = 1, cube%nz ),&
            ((( cube%u(i,j,k,1), i = 1, cube%nx ), j = 1, cube%ny ), k = 1, cube%nz ),&
            ((( cube%u(i,j,k,2), i = 1, cube%nx ), j = 1, cube%ny ), k = 1, cube%nz ),&
            ((( cube%u(i,j,k,3), i = 1, cube%nx ), j = 1, cube%ny ), k = 1, cube%nz ),&
            ((( cube%phi(i,j,k), i = 1, cube%nx ), j = 1, cube%ny ), k = 1, cube%nz )
            close(10)
            
        return 
    end subroutine output_cnt_q

end module field_cube