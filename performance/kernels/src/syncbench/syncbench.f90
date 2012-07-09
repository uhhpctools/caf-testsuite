module util
    private
    public co_maxval
    public sync_comm
#ifdef TEST_MPI
    public sync_images_mpi
#endif

    integer :: sync_comm


    contains
        subroutine co_maxval(t, tt)
            real :: t[*], tt
            integer :: i

            sync all

            tt = t
            if (this_image() == 1) then
                do i=2,num_images()
                  t = max(t, t[i])
                end do
            end if

            sync all

            tt = t[1]

            sync all

        end subroutine co_maxval

#ifdef TEST_MPI
        subroutine sync_images_mpi(list)
            implicit none

            include 'mpif.h'

            integer :: list(:)

            integer, parameter :: SYNC_IMAGES_TAG = 555
            integer :: length
            integer :: i
            integer :: ierr
            integer :: dummy
            integer :: stat1(MPI_STATUS_SIZE)
            integer, allocatable :: req(:), stat(:,:)


            length = SIZE(list)

            if (length > 1) then
                allocate ( req(2*length), stat(MPI_STATUS_SIZE,2*length) )

                req = MPI_REQUEST_NULL

                !----------------------------------
                ! post receives
                !----------------------------------

                do i=1,length
                  call MPI_IRECV(dummy, 0, MPI_BYTE, list(i)-1, &
                                 SYNC_IMAGES_TAG, sync_comm, req(2*i-1), &
                                 ierr)
                end do

                !----------------------------------
                ! post sends
                !----------------------------------

                do i=1,length
                  call MPI_ISEND(dummy, 0, MPI_BYTE, list(i)-1, &
                                SYNC_IMAGES_TAG, sync_comm, req(2*i),ierr)
                end do

                !----------------------------------
                ! wait on all requests to complete
                !----------------------------------

                call MPI_WAITALL(2*length, req, stat, ierr)

            else if (length == 1) then
                !----------------------------------
                ! use sendrecv
                !----------------------------------

                call MPI_SENDRECV_REPLACE(dummy, 0, MPI_BYTE, list(1)-1, &
                                          SYNC_IMAGES_TAG, list(1)-1, &
                                          SYNC_IMAGES_TAG, sync_comm, &
                                          stat1, ierr)

            else
                error stop "sync_images: invalid list length"
            end if

        end subroutine sync_images_mpi
#endif
end module util

program sync_test
    use,intrinsic :: iso_fortran_env
    use :: util

    implicit none

#ifdef TEST_MPI
    include 'mpif.h'
#endif

    integer :: N[*],M
    character(len=20) :: arg
    integer :: ti,ni,i,j
    real    :: start_time, end_time, t[*], total_time
    integer :: start_wtime, end_wtime, wt[*], total_wtime, rate
    integer :: nargs
    integer :: ierr
    integer :: r, q, log2_q, step, partner

#ifdef TEST_MPI
    call MPI_Init(ierr)

    call MPI_COMM_DUP(MPI_COMM_WORLD, sync_comm, ierr)
#endif

    ti = this_image()
    ni = num_images()

    if (ti == 1) then
      nargs = command_argument_count()
      if (nargs /= 1) then
        print *, "Enter #iterations: "
        read (INPUT_UNIT, '(i20)') N
        do i = 2, ni
            N[i] = N
        end do
      else
        call get_command_argument(1, arg)
        read (arg,'(i20)') N
        do i = 2, ni
            N[i] = N
        end do
      end if
    end if

    sync all

                !----------------------------------
                ! MANY-TO-1 with SYNC IMAGES
                !----------------------------------

    M = N / 100

    if (ti == 1) then
        print *, " testing many-to-1 with sync images (", N, " iterations)"
    end if

    call system_clock(start_wtime, rate)

    do i = 1, N
        if (ti == 1) then
            sync images(*)
        else
            sync images(1)
        end if
#ifdef PROGRESS_BAR
        if (ti==1 .and. mod(i,M)==1) then
          write (*,"('.')",advance="no")
          call flush(OUTPUT_UNIT)
        end if
#endif
    end do

    call system_clock(end_wtime)

    if (ti == 1) then
        print *, " done!"
    end if

    t = end_time - start_time

    call co_maxval(t, total_time)

    if (ti == 1) then
        write (*, "(A40, f8.3, ' us.')") &
               "Time: ", 1000000.0/N*(end_wtime - start_wtime)/(1.0*rate)
    end if


    sync all
                !----------------------------------
                ! RING WITH SYNC IMAGES
                !----------------------------------

    if (ti == 1) then
        print *, " testing ring with sync images (", N, " iterations)"
    end if

    call system_clock(start_wtime, rate)

    do i = 1, N
        if (ti == 1) then
            sync images([2,ni])
        else if (ti == ni) then
            sync images([ni-1,1])
        else
            sync images([ti-1,ti+1])
        end if
#ifdef PROGRESS_BAR
        if (ti==1 .and. mod(i,M)==1) then
          write (*,"('.')",advance="no")
          call flush(OUTPUT_UNIT)
        end if
#endif
    end do

    call system_clock(end_wtime)
    if (ti == 1) then
        print *, " done!"
    end if

    t = end_time - start_time

    call co_maxval(t, total_time)

    if (ti == 1) then
        write (*, "(A40, f8.3, ' us.')") &
               "Time: ", 1000000.0/N*(end_wtime - start_wtime)/(1.0*rate)
    end if

    sync all
                !----------------------------------
                ! ALL2ALL with SYNC IMAGES
                !----------------------------------

    if (ti == 1) then
        print *, " testing all-to-all with sync images (", N, " iterations)"
    end if

    call system_clock(start_wtime, rate)

    do i = 1, N
        sync images ( * )
#ifdef PROGRESS_BAR
        if (ti==1 .and. mod(i,M)==1) then
          write (*,"('.')",advance="no")
          call flush(OUTPUT_UNIT)
        end if
#endif
    end do

    call system_clock(end_wtime)
    if (ti == 1) then
        print *, " done!"
    end if

    t = end_time - start_time

    call co_maxval(t, total_time)

    if (ti == 1) then
        write (*, "(A40, f8.3, ' us.')") &
               "Time: ", 1000000.0/N*(end_wtime - start_wtime)/(1.0*rate)
    end if

    sync all

            !-------------------------------------------------
            ! RECURSIVE DOUBLING WITH SYNC IMAGES
            !-------------------------------------------------

    if (ti == 1) then
        print *, " testing recursive doubling with sync images (", N, " iterations)"
    end if


    call system_clock(start_wtime, rate)

    !-------------------------------------------------
    ! Find greatest power of 2 less than p, q
    !-------------------------------------------------
    q = 1
    log2_q = 0
    do while (2*q <= ni)
      q = 2*q
      log2_q = log2_q + 1
    end do

    r = ni - q

    do i = 1, N
        !--------------------------------------------------
        ! first r and last r images synchronize
        !--------------------------------------------------
        if (ti > q) then
            partner = ti - q
            sync images (partner)
        else if (ti <= r) then
            partner = ti + q
            sync images (partner)
        end if
        !--------------------------------------------------
        ! first q images do a recursive doubling synchronization
        ! pattern
        !--------------------------------------------------
        if (ti <= q) then
            step = 1
            do while (step < q)
            if (mod(ti-1,2*step) < step) then
                partner = ti + step
                sync images (partner)
            else
                partner = ti - step
                sync images (partner)
            end if
            step = step*2
            end do
        end if
        !--------------------------------------------------
        ! first r and last r images synchronize
        !--------------------------------------------------
        if (ti <= r) then
            partner = ti + q
            sync images (partner)
        else if (ti > q) then
            partner = ti - q
            sync images (partner)
        end if

#ifdef PROGRESS_BAR
        if (ti==1 .and. mod(i,M)==1) then
          write (*,"('.')",advance="no")
          call flush(OUTPUT_UNIT)
        end if
#endif
    end do

    call system_clock(end_wtime)
    if (ti == 1) then
        print *, " done!"
    end if

    t = end_time - start_time

    call co_maxval(t, total_time)

    if (ti == 1) then
        write (*, "(A40, f8.3, ' us.')") &
               "Time: ", 1000000.0/N*(end_wtime - start_wtime)/(1.0*rate)
    end if

    sync all

#ifdef TEST_MPI
                !----------------------------------
                ! MANY-TO-1 with SYNC_IMAGES_MPI
                !----------------------------------

    M = N / 100

    if (ti == 1) then
        print *, " testing many-to-1 with sync_images_mpi (", N, " iterations)"
    end if

    call system_clock(start_wtime, rate)

    do i = 1, N
        if (ti == 1) then
            call sync_images_mpi( [(j, j=1,num_images())] )
        else
            call sync_images_mpi([1])
        end if
#ifdef PROGRESS_BAR
        if (ti==1 .and. mod(i,M)==1) then
          write (*,"('.')",advance="no")
          call flush(OUTPUT_UNIT)
        end if
#endif
    end do

    call system_clock(end_wtime)

    if (ti == 1) then
        print *, " done!"
    end if

    t = end_time - start_time

    call co_maxval(t, total_time)

    if (ti == 1) then
        write (*, "(A40, f8.3, ' us.')") &
               "Time: ", 1000000.0/N*(end_wtime - start_wtime)/(1.0*rate)
    end if


    sync all
                !----------------------------------
                ! RING WITH SYNC_IMAGES_MPI
                !----------------------------------

    if (ti == 1) then
        print *, " testing ring with sync_images_mpi (", N, " iterations)"
    end if

    call system_clock(start_wtime, rate)

    do i = 1, N
        if (ti == 1) then
            call sync_images_mpi([2,ni])
        else if (ti == ni) then
            call sync_images_mpi([ni-1,1])
        else
            call sync_images_mpi([ti-1,ti+1])
        end if
#ifdef PROGRESS_BAR
        if (ti==1 .and. mod(i,M)==1) then
          write (*,"('.')",advance="no")
          call flush(OUTPUT_UNIT)
        end if
#endif
    end do

    call system_clock(end_wtime)
    if (ti == 1) then
        print *, " done!"
    end if

    t = end_time - start_time

    call co_maxval(t, total_time)

    if (ti == 1) then
        write (*, "(A40, f8.3, ' us.')") &
               "Time: ", 1000000.0/N*(end_wtime - start_wtime)/(1.0*rate)
    end if

    sync all
                !----------------------------------
                ! ALL2ALL with SYNC_IMAGES_MPI
                !----------------------------------

    if (ti == 1) then
        print *, " testing all-to-all with sync_images_mpi (", N, " iterations)"
    end if

    call system_clock(start_wtime, rate)
    call cpu_time(start_time)

    do i = 1, N
    call sync_images_mpi( [(j, j=1,num_images())] )
#ifdef PROGRESS_BAR
        if (ti==1 .and. mod(i,M)==1) then
          write (*,"('.')",advance="no")
          call flush(OUTPUT_UNIT)
        end if
#endif
    end do

    call cpu_time(end_time)
    call system_clock(end_wtime)
    if (ti == 1) then
        print *, " done!"
    end if

    t = end_time - start_time

    call co_maxval(t, total_time)

    if (ti == 1) then
        write (*, "(A40, f8.3, ' seconds.')") &
               "Time:: ", 1000000.0/N*(end_wtime - start_wtime)/(1.0*rate)
    end if

    sync all

        !-------------------------------------------------
        ! RECURSIVE DOUBLING WITH SYNC_IMAGES_MPI
        !-------------------------------------------------

    if (ti == 1) then
        print *, " testing recursive doubling with sync_images_mpi (", N, " iterations)"
    end if


    call system_clock(start_wtime, rate)

    !-------------------------------------------------
    ! Find greatest power of 2 less than p, q
    !-------------------------------------------------
    q = 1
    log2_q = 0
    do while (2*q <= ni)
      q = 2*q
      log2_q = log2_q + 1
    end do

    r = ni - q

    do i = 1, N
        !--------------------------------------------------
        ! first r and last r images synchronize
        !--------------------------------------------------
        if (ti > q) then
            partner = ti - q
            call sync_images_mpi([partner])
        else if (ti <= r) then
            partner = ti + q
            call sync_images_mpi([partner])
        end if
        !--------------------------------------------------
        ! first q images do a recursive doubling synchronization
        ! pattern
        !--------------------------------------------------
        if (ti <= q) then
            step = 1
            do while (step < q)
            if (mod(ti-1,2*step) < step) then
                partner = ti + step
                call sync_images_mpi([partner])
            else
                partner = ti - step
                call sync_images_mpi([partner])
            end if
            step = step*2
            end do
        end if
        !--------------------------------------------------
        ! first r and last r images synchronize
        !--------------------------------------------------
        if (ti <= r) then
            partner = ti + q
            call sync_images_mpi([partner])
        else if (ti > q) then
            partner = ti - q
            call sync_images_mpi([partner])
        end if

#ifdef PROGRESS_BAR
        if (ti==1 .and. mod(i,M)==1) then
          write (*,"('.')",advance="no")
          call flush(OUTPUT_UNIT)
        end if
#endif
    end do

    call system_clock(end_wtime)
    if (ti == 1) then
        print *, " done!"
    end if

    t = end_time - start_time

    call co_maxval(t, total_time)

    if (ti == 1) then
        write (*, "(A40, f8.3, ' us.')") &
               "Time: ", 1000000.0/N*(end_wtime - start_wtime)/(1.0*rate)
    end if

#endif

    sync all

        !-------------------------------------------------
        ! GLOBAL BARRIER (SYNC ALL)
        !-------------------------------------------------


    if (ti == 1) then
        print *, " testing with sync all ... (", N, " iterations)"
    end if

    call system_clock(start_wtime, rate)

    do i = 1, N
        sync all
        !if (ti==1 .and. mod(i,100)==1) print *,i
#ifdef PROGRESS_BAR
        if (ti==1 .and. mod(i,M)==1) then
          write (*,"('.')",advance="no")
          call flush(OUTPUT_UNIT)
        end if
#endif
    end do

    call system_clock(end_wtime)
    if (ti == 1) then
        print *, " done!"
    end if

    t = end_time - start_time

    call co_maxval(t, total_time)

    if (ti == 1) then
        write (*, "(A40, f8.3, ' us.')") &
               "Time: ", 1000000.0/N*(end_wtime - start_wtime)/(1.0*rate)
    end if

    sync all

#ifdef TEST_MPI

        !-------------------------------------------------
        ! GLOBAL BARRIER (MPI_Barrier)
        !-------------------------------------------------

    if (ti == 1) then
        print *, " testing with MPI_Barrier ... (", N, " iterations)"
    end if

    call system_clock(start_wtime, rate)

    do i = 1, N
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        !if (ti==1 .and. mod(i,100)==1) print *,i
#ifdef PROGRESS_BAR
        if (ti==1 .and. mod(i,M)==1) then
          write (*,"('.')",advance="no")
          call flush(OUTPUT_UNIT)
        end if
#endif
    end do

    call system_clock(end_wtime)
    if (ti == 1) then
        print *, " done!"
    end if

    t = end_time - start_time

    call co_maxval(t, total_time)

    if (ti == 1) then
        write (*, "(A40, f8.3, ' us.')") &
               "Time: ", 1000000.0/N*(end_wtime - start_wtime)/(1.0*rate)
    end if

    sync all

    call MPI_Finalize(ierr)
#endif

end program
