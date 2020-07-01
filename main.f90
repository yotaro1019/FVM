program main
    use params
    use field_cube
    implicit none
    
    call param%read_param()
    call cube%field_init()
    call cube%out_cnt_g()
    call cube%out_cnt_q()


    stop
end program main