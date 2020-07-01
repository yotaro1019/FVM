module params
    implicit none
        type t_parameters
            integer :: nx, ny, nz
            real(8) :: dx, dy, dz

            contains
            procedure:: read_param => read_param

        end type t_parameters

        type(t_parameters) :: param 

contains

    subroutine read_param(param)
        implicit none
        class(t_parameters) :: param 
        
        param%nx = 10
        param%ny = 10
        param%nz = 10
        param%dx = 1
        param%dy = 1
        param%dz = 1



        write(*,*) "Complete Read Parameters"
        write(*,*) param%nx, " ", param%ny, " ", param%nz
        write(*,*) param%dx, " ", param%dy, " ", param%dz
        return 
    end subroutine read_param

end module params