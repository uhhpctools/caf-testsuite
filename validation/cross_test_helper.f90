module cross_test

    IMPLICIT NONE
    integer:: cross_err[*]

contains

    subroutine calc_ori()
        if (this_image() == 1 .and. cross_err[1] > 0) then
            STOP 1
        end if
    end subroutine

    subroutine calc (tot_NITER)
        integer :: tot_NITER, err_cnt
        integer :: size, rank, i
        integer :: percent=0
        real :: mean,sumsq, sqsum, std_dev, root_size,ub,lb,n_size,t_val
        real , allocatable :: mat(:,:)
        character(len=30) :: bound_str
        character(len=100) :: cmd_str

        size =  num_images()
        rank = this_image()
        err_cnt = cross_err[1]
        n_size=tot_NITER

        if (rank == 1) then
            ! If cross test gives an error, the semantics of the validated CAF construct passes.
            write(bound_str,"(A,I6,A,I6)") "Passed:", err_cnt,"/",tot_NITER
            write(cmd_str,"(A,A,A)") "echo ",trim(adjustl(bound_str))," > conf.temp"
            call system(cmd_str)
            call  exit(2)
        endif

    end subroutine calc


end module cross_test
