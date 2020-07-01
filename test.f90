program test
    implicit none
    integer :: i,j,k,l
    real(8) :: f(3,3)


    do i = 1, 3
        do j = 1, 3
            f(i,j) = ((i-1)*3)+j
            write(*,*) f(i,j)
        end do
    end do
    write(*,*) (( f(i,j), i= 1, 3), j=1,3)
    write(*,*) "======"
    write(*,*) ( f(i,1), i= 1, 3)
    stop
end program test