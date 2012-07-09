!
! Microbenchmarks for CAF
!
! (C) HPCTools Group, University of Houston
!

module caf_microbenchmarks
    use, intrinsic :: iso_fortran_env
    implicit none

    integer, parameter :: BARRIER = 1
    integer, parameter :: P2P = 2

    integer, parameter :: TARGET_STRIDED = 1
    integer, parameter :: ORIGIN_STRIDED = 2
    integer, parameter :: BOTH_STRIDED = 3

    integer, parameter :: BUFFER_SIZE = 1024*1024
    integer, parameter :: LAT_NITER = 10000
    integer, parameter :: BW_NITER = 1000
    integer, parameter :: RED_NITER = 100

    integer, parameter :: TIMEOUT = 5

    integer, parameter :: ELEM_SIZE = 4

    integer, parameter :: NUM_STATS = 32

#ifndef NO_LOCK_SUPPORT
    type(lock_type) :: image_lock[*]
#endif
    integer :: send_buffer(BUFFER_SIZE)[*]
    integer :: recv_buffer(BUFFER_SIZE)[*]
    double precision, allocatable :: stats_buffer(:)[:]

    integer :: partner
    integer :: numi

    contains


    function MY_WTIME()
        double precision :: MY_WTIME
        double precision :: t

        call wtime(t)

        MY_WTIME = t
    end function MY_WTIME


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !                 SYNCHRONIZATION ROUTINES
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine do_sync(sync)
        integer, intent(in) :: sync

        if (sync == BARRIER) then
            sync all
        else if (sync == P2P) then
            sync images (partner)
        end if
    end subroutine

#ifdef LATENCY_TESTS
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !                       LATENCY TESTS
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine run_sync_latency_test(sync)
        implicit none
        integer :: sync
        integer :: num_pairs
        double precision :: t1, t2
        double precision :: latency_other

        integer :: ti, ni, i

        ti = this_image()
        ni = num_images()

        num_pairs = numi / 2

        if (ti == 1) then
            if (sync == BARRIER) then
                write (*,'(//,"SYNC Latency: (",I0," pairs, ",A,")")') &
                    num_pairs, "barrier"
            else
                write (*,'(//, "SYNC Latency: (",I0," pairs, ",A,")")') &
                    num_pairs, "p2p"
            end if
        end if

        t1 = MY_WTIME()
        do i = 1, LAT_NITER
          call do_sync(sync)
        end do
        t2 = MY_WTIME()

        stats_buffer(1) = 1000000*(t2-t1)/(LAT_NITER)

        sync all

        if (ti == 1) then
            ! collect stats from other nodes
            do i = 2, num_pairs
              latency_other = stats_buffer(1)[i]
              stats_buffer(1) = stats_buffer(1) + latency_other
            end do
            write (*, '(F20.8, " us")') stats_buffer(1)/num_pairs
        end if

    end subroutine run_sync_latency_test

    subroutine run_putget_latency_test()
        implicit none
        integer :: num_pairs
        double precision :: t1, t2
        double precision :: latency_other

        integer :: ti, ni, i

        ti = this_image()
        ni = num_images()

        num_pairs = numi / 2

        if (ti == 1) then
            write (*,'(//,"Put-Get Latency: (",I0," pairs)")') &
                 num_pairs
        end if

        if (ti < partner) then
            t1 = MY_WTIME()
            do i = 1, LAT_NITER
              recv_buffer(1)[partner] = send_buffer(1)
              recv_buffer(1) = recv_buffer(1)[partner]
            end do
            t2 = MY_WTIME()

            stats_buffer(1) = 1000000*(t2-t1)/(LAT_NITER)
        end if

        sync all

        if (ti == 1) then
            ! collect stats from other nodes
            do i = 2, num_pairs
              latency_other = stats_buffer(1)[i]
              stats_buffer(1) = stats_buffer(1) + latency_other
            end do
            write (*, '(F20.8, " us")') stats_buffer(1)/num_pairs
        end if

    end subroutine run_putget_latency_test

    subroutine run_putput_latency_test(sync)
        implicit none
        integer :: sync

        integer :: num_pairs
        double precision :: t1, t2
        double precision :: latency_other

        integer :: ti, ni, i

        ti = this_image()
        ni = num_images()

        num_pairs = numi / 2

        if (ti == 1) then
            if (sync == BARRIER) then
                write (*,'(//,"Put-Put Latency: (",I0," pairs, ",A,")")') &
                    num_pairs, "barrier"
            else
                write (*,'(//, "Put-Put Latency: (",I0," pairs, ",A,")")') &
                    num_pairs, "p2p"
            end if
        end if

        if (ti < partner) then
            t1 = MY_WTIME()
            do i = 1, LAT_NITER
              recv_buffer(1)[partner] = send_buffer(1)
              call do_sync(sync)
              call do_sync(sync)
            end do
            t2 = MY_WTIME()

            stats_buffer(1) = 1000000*(t2-t1)/(LAT_NITER)
        else if (ti <= numi) then
            t1 = MY_WTIME()
            do i = 1, LAT_NITER
              call do_sync(sync)
              recv_buffer(1)[partner] = send_buffer(1)
              call do_sync(sync)
            end do
            t2 = MY_WTIME()

            stats_buffer(1) = 1000000*(t2-t1)/(LAT_NITER)
        end if

        sync all

        if (ti == 1) then
            ! collect stats from other nodes
            do i = 2, numi
              latency_other = stats_buffer(1)[i]
              stats_buffer(1) = stats_buffer(1) + latency_other
            end do
            write (*, '(F20.8, " us")') stats_buffer(1)/numi
        end if

    end subroutine run_putput_latency_test

    subroutine run_getget_latency_test(sync)
        implicit none
        integer :: sync

        integer :: num_pairs
        double precision :: t1, t2
        double precision :: latency_other

        integer :: ti, ni, i

        ti = this_image()
        ni = num_images()

        num_pairs = numi / 2

        if (ti == 1) then
            if (sync == BARRIER) then
                write (*,'(//,"Get-Get Latency: (",I0," pairs, ",A,")")') &
                    num_pairs, "barrier"
            else
                write (*,'(//,"Get-Get Latency: (",I0," pairs, ",A,")")') &
                    num_pairs, "p2p"
            end if
        end if

        if (ti < partner) then
            t1 = MY_WTIME()
            do i = 1, LAT_NITER
              recv_buffer(1) = send_buffer(1)[partner]
              call do_sync(sync)
              call do_sync(sync)
            end do
            t2 = MY_WTIME()

            stats_buffer(1) = 1000000*(t2-t1)/(LAT_NITER)
        else if (ti <= numi) then
            t1 = MY_WTIME()
            do i = 1, LAT_NITER
              call do_sync(sync)
              recv_buffer(1) = send_buffer(1)[partner]
              call do_sync(sync)
            end do
            t2 = MY_WTIME()

            stats_buffer(1) = 1000000*(t2-t1)/(LAT_NITER)
        end if

        sync all

        if (ti == 1) then
            ! collect stats from other nodes
            do i = 2, numi
              latency_other = stats_buffer(1)[i]
              stats_buffer(1) = stats_buffer(1) + latency_other
            end do
            write (*, '(F20.8, " us")') stats_buffer(1)/numi
        end if

    end subroutine run_getget_latency_test
#endif

#ifdef BW_TESTS
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !                   BANDWIDTH TESTS
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine run_put_bw_test()
        implicit none
        double precision :: t1, t2
        integer :: ti, ni, nrep
        integer :: num_stats
        integer :: num_pairs
        integer :: blksize
        integer :: i

        ti = this_image()
        ni = num_images()
        num_pairs = numi / 2

        if (ti == 1) then
            write (*,'(//,"1-Way Put Bandwith: (",I0," pairs)")') &
                num_pairs
            write (*,'(A20, A20, A20)') "blksize", "nrep", "bandwidth"
        end if

        num_stats = 1
        blksize = 1
        do while (blksize <= BUFFER_SIZE)
          nrep = BW_NITER

          if (ti < partner) then
              t1 = MY_WTIME()
              do i = 1, nrep
                recv_buffer(1:blksize)[partner] = send_buffer(1:blksize)
                if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                  nrep = i
                  exit
                end if
              end do
              t2 = MY_WTIME()

              stats_buffer(num_stats) = &
                  dble(blksize)*ELEM_SIZE*nrep/(1024*1024*(t2-t1))
          end if

          sync all

          if (ti == 1) then
              do i = 2, num_pairs
                  stats_buffer(num_stats) = stats_buffer(num_stats) + &
                                            stats_buffer(num_stats)[i]
              end do
              write (*, '(I20,I20,F17.3, " MB/s")') &
                     blksize, nrep, stats_buffer(num_stats)/num_pairs
          end if

          num_stats = num_stats + 1
          blksize = blksize * 2
        end do

    end subroutine run_put_bw_test

    subroutine run_get_bw_test()
        implicit none
        double precision :: t1, t2
        integer :: ti, ni, nrep
        integer :: num_stats
        integer :: num_pairs
        integer :: blksize
        integer :: i

        ti = this_image()
        ni = num_images()
        num_pairs = numi / 2

        if (ti == 1) then
            write (*,'(//,"1-Way Get Bandwith: (",I0," pairs)")') &
                num_pairs
            write (*,'(A20, A20, A20)') "blksize", "nrep", "bandwidth"
        end if

        num_stats = 1
        blksize = 1
        do while (blksize <= BUFFER_SIZE)
          nrep = BW_NITER

          if (ti < partner) then
              t1 = MY_WTIME()
              do i = 1, nrep
                recv_buffer(1:blksize) = send_buffer(1:blksize)[partner]
                if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                  nrep = i
                  exit
                end if
              end do
              t2 = MY_WTIME()

              stats_buffer(num_stats) = &
                  dble(blksize)*ELEM_SIZE*nrep/(1024*1024*(t2-t1))
          end if

          sync all

          if (ti == 1) then
              do i = 2, num_pairs
                  stats_buffer(num_stats) = stats_buffer(num_stats) + &
                                            stats_buffer(num_stats)[i]
              end do
              write (*, '(I20,I20,F17.3, " MB/s")') &
                      blksize, nrep, stats_buffer(num_stats)/num_pairs
          end if

          num_stats = num_stats + 1
          blksize = blksize * 2
        end do
    end subroutine run_get_bw_test

#ifndef NO_LOCK_SUPPORT
    subroutine run_rand_put_bw_test()
        implicit none
        double precision :: t1, t2
        integer :: ti, ni, nrep
        integer :: num_stats
        integer :: num_pairs
        integer :: blksize
        integer :: i
        integer :: rand_seed, rand_index, rand_image
        real    :: real_rand_num

        ti = this_image()
        ni = num_images()
        num_pairs = numi / 2

        if (ti == 1) then
            write (*,'(//,"Random Put Bandwith")')
            write (*,'(A20, A20, A20)') "blksize", "nrep", "bandwidth"
        end if

        num_stats = 1
        blksize = 1
        do while (blksize <= BUFFER_SIZE/2)
          nrep = BW_NITER

          t1 = MY_WTIME()
          do i = 1, nrep
            rand_seed = i*this_image()
            call random_seed(rand_seed)
            call random_number(real_rand_num)
            rand_index = INT(real_rand_num*BUFFER_SIZE/2)
            call random_number(real_rand_num)
            rand_image = INT(real_rand_num*num_images())+1
            lock (image_lock[rand_image])
            recv_buffer(rand_index:rand_index+blksize-1)[rand_image] = &
                send_buffer(1:blksize)
            unlock (image_lock[rand_image])
            if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
              nrep = i
              exit
            end if
          end do
          t2 = MY_WTIME()

          stats_buffer(num_stats) = &
              dble(blksize)*ELEM_SIZE*nrep/(1024*1024*(t2-t1))

          sync all

          if (ti == 1) then
              do i = 2, num_images()
                  stats_buffer(num_stats) = stats_buffer(num_stats) + &
                                            stats_buffer(num_stats)[i]
              end do
              write (*, '(I20,I20,F17.3, " MB/s")') &
                     blksize, nrep, stats_buffer(num_stats)/num_images()
          end if

          num_stats = num_stats + 1
          blksize = blksize * 2
        end do

    end subroutine run_rand_put_bw_test

    subroutine run_rand_get_bw_test()
        implicit none
        double precision :: t1, t2
        integer :: ti, ni, nrep
        integer :: num_stats
        integer :: num_pairs
        integer :: blksize
        integer :: i
        integer :: rand_seed, rand_index, rand_image
        real    :: real_rand_num

        ti = this_image()
        ni = num_images()
        num_pairs = numi / 2

        if (ti == 1) then
            write (*,'(//,"Random Get Bandwith")')
            write (*,'(A20, A20, A20)') "blksize", "nrep", "bandwidth"
        end if

        num_stats = 1
        blksize = 1
        do while (blksize <= BUFFER_SIZE)
          nrep = BW_NITER

          t1 = MY_WTIME()
          do i = 1, nrep
            rand_seed = i*this_image()
            call random_seed(rand_seed)
            call random_number(real_rand_num)
            rand_index = INT(real_rand_num*BUFFER_SIZE/2)
            call random_number(real_rand_num)
            rand_image = INT(real_rand_num*num_images())+1
            lock (image_lock[rand_image])
            recv_buffer(1:blksize) = &
                 send_buffer(rand_index:rand_index+blksize-1)[rand_image]
            unlock (image_lock[rand_image])
            if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
              nrep = i
              exit
            end if
          end do
          t2 = MY_WTIME()

          stats_buffer(num_stats) = &
              dble(blksize)*ELEM_SIZE*nrep/(1024*1024*(t2-t1))

          sync all

          if (ti == 1) then
              do i = 2, num_images()
                  stats_buffer(num_stats) = stats_buffer(num_stats) + &
                                            stats_buffer(num_stats)[i]
              end do
              write (*, '(I20,I20,F17.3, " MB/s")') &
                     blksize, nrep, stats_buffer(num_stats)/num_images()
          end if

          num_stats = num_stats + 1
          blksize = blksize * 2
        end do
    end subroutine run_rand_get_bw_test
#endif


    subroutine run_put_bw_bidir_test()
        implicit none
        double precision :: t1, t2
        integer :: ti, ni, nrep
        integer :: num_stats
        integer :: num_pairs
        integer :: blksize
        integer :: i

        ti = this_image()
        ni = num_images()
        num_pairs = numi / 2

        if (ti == 1) then
            write (*,'(//,"2-Way Put Bandwith: (",I0," pairs)")') &
                num_pairs
            write (*,'(A20, A20, A20)') "blksize", "nrep", "bandwidth"
        end if

        num_stats = 1
        blksize = 1
        do while (blksize <= BUFFER_SIZE)
          nrep = BW_NITER

          t1 = MY_WTIME()
          do i = 1, nrep
            recv_buffer(1:blksize)[partner] = send_buffer(1:blksize)
            if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
              nrep = i
              exit
            end if
          end do
          t2 = MY_WTIME()

          stats_buffer(num_stats) = &
              dble(blksize)*ELEM_SIZE*nrep/(1024*1024*(t2-t1))

          sync all

          if (ti == 1) then
              do i = 2, numi
                  stats_buffer(num_stats) = stats_buffer(num_stats) + &
                                            stats_buffer(num_stats)[i]
              end do
              write (*, '(I20,I20,F17.3, " MB/s")') &
                     blksize, nrep, stats_buffer(num_stats)/numi
          end if

          num_stats = num_stats + 1
          blksize = blksize * 2
        end do
    end subroutine run_put_bw_bidir_test

    subroutine run_get_bw_bidir_test()
        implicit none
        double precision :: t1, t2
        integer :: ti, ni, nrep
        integer :: num_stats
        integer :: num_pairs
        integer :: blksize
        integer :: i

        ti = this_image()
        ni = num_images()
        num_pairs = numi / 2

        if (ti == 1) then
            write (*,'(//,"2-Way Get Bandwith: (",I0," pairs)")') &
                num_pairs
            write (*,'(A20, A20, A20)') "blksize", "nrep", "bandwidth"
        end if

        num_stats = 1
        blksize = 1
        do while (blksize <= BUFFER_SIZE)
          nrep = BW_NITER

          t1 = MY_WTIME()
          do i = 1, nrep
            recv_buffer(1:blksize) = send_buffer(1:blksize)[partner]
            if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
              nrep = i
              exit
            end if
          end do
          t2 = MY_WTIME()

          stats_buffer(num_stats) = &
              dble(blksize)*ELEM_SIZE*nrep/(1024*1024*(t2-t1))

          sync all

          if (ti == 1) then
              do i = 2, numi
                  stats_buffer(num_stats) = stats_buffer(num_stats) + &
                                            stats_buffer(num_stats)[i]
              end do
              write (*, '(I20,I20,F17.3, " MB/s")') &
                     blksize, nrep, stats_buffer(num_stats)/numi
          end if

          num_stats = num_stats + 1
          blksize = blksize * 2
        end do
    end subroutine run_get_bw_bidir_test

#endif



#ifdef STRIDED_BW_TESTS

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !                  STRIDED BANDWIDTH TESTS
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine run_strided_put_bw_test(strided)
        implicit none

        integer, intent(in) :: strided

        character (len=15) :: strided_label(3)
        double precision :: t1, t2
        integer :: ti, ni, nrep
        integer :: num_stats
        integer :: num_pairs
        integer, parameter :: MAX_COUNT = 32*1024
        integer, parameter :: MAX_BLKSIZE = BUFFER_SIZE
        integer, parameter :: MAX_STRIDE = MAX_BLKSIZE / MAX_COUNT
        integer :: stride,extent
        integer :: i

        ti = this_image()
        ni = num_images()
        num_pairs = numi / 2

        if (ti == 1) then
            strided_label(1) = "Target Strided"
            strided_label(2) = "Origin Strided"
            strided_label(3) = "Both Strided"

            write (*,'(//,"1-Way ",A," Put Bandwith: (",I0," pairs)")') &
                strided_label(strided), num_pairs
            write (*,'(A20, A20, A20, A20)') "count", "stride", "nrep", "bandwidth"
        end if

        num_stats = 1
        stride = 1
        do while (stride <= MAX_STRIDE)
          nrep = BW_NITER

          if (ti < partner) then
              if (strided == TARGET_STRIDED) then
                  t1 = MY_WTIME()
                  do i = 1, nrep
                    extent = MAX_COUNT*stride
                    recv_buffer(1:extent:stride)[partner] = send_buffer(1:MAX_COUNT)
                    if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                      nrep = i
                      exit
                    end if
                  end do
                  t2 = MY_WTIME()
              else if (strided == ORIGIN_STRIDED) then
                  t1 = MY_WTIME()
                  do i = 1, nrep
                    extent = MAX_COUNT*stride
                    recv_buffer(1:MAX_COUNT)[partner] = send_buffer(1:extent:stride)
                    if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                      nrep = i
                      exit
                    end if
                  end do
                  t2 = MY_WTIME()
              else
                  t1 = MY_WTIME()
                  do i = 1, nrep
                    extent = MAX_COUNT*stride
                    recv_buffer(1:extent:stride)[partner] = &
                                 send_buffer(1:extent:stride)
                    if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                      nrep = i
                      exit
                    end if
                  end do
                  t2 = MY_WTIME()
              end if

              stats_buffer(num_stats) = &
                  dble(MAX_COUNT)*ELEM_SIZE*nrep/(1024*1024*(t2-t1))
          end if

          sync all

          if (ti == 1) then
              do i = 2, num_pairs
                  stats_buffer(num_stats) = stats_buffer(num_stats) + &
                                            stats_buffer(num_stats)[i]
              end do
              write (*, '(I20,I20,I20,F17.3, " MB/s")') &
                     MAX_COUNT, stride, nrep, stats_buffer(num_stats)/num_pairs
          end if

          num_stats = num_stats + 1
          stride = stride * 2
        end do
    end subroutine run_strided_put_bw_test

    subroutine run_strided_get_bw_test(strided)
        implicit none

        integer, intent(in) :: strided

        character (len=15) :: strided_label(3)
        double precision :: t1, t2
        integer :: ti, ni, nrep
        integer :: num_stats
        integer :: num_pairs
        integer, parameter :: MAX_COUNT = 32*1024
        integer, parameter :: MAX_BLKSIZE = BUFFER_SIZE
        integer, parameter :: MAX_STRIDE = MAX_BLKSIZE / MAX_COUNT
        integer :: stride,extent
        integer :: i

        ti = this_image()
        ni = num_images()
        num_pairs = numi / 2

        if (ti == 1) then
            strided_label(1) = "Target Strided"
            strided_label(2) = "Origin Strided"
            strided_label(3) = "Both Strided"

            write (*,'(//,"1-Way ",A," Get Bandwith: (",I0," pairs)")') &
                strided_label(strided), num_pairs
            write (*,'(A20, A20, A20, A20)') "count", "stride", "nrep", "bandwidth"
        end if

        num_stats = 1
        stride = 1
        do while (stride <= MAX_STRIDE)
          nrep = BW_NITER

          if (ti < partner) then
              if (strided == TARGET_STRIDED) then
                  t1 = MY_WTIME()
                  do i = 1, nrep
                    extent = MAX_COUNT*stride
                    recv_buffer(1:MAX_COUNT) = send_buffer(1:extent:stride)[partner]
                    if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                      nrep = i
                      exit
                    end if
                  end do
                  t2 = MY_WTIME()
              else if (strided == ORIGIN_STRIDED) then
                  t1 = MY_WTIME()
                  do i = 1, nrep
                    extent = MAX_COUNT*stride
                    recv_buffer(1:extent:stride) = send_buffer(1:MAX_COUNT)[partner]
                    if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                      nrep = i
                      exit
                    end if
                  end do
                  t2 = MY_WTIME()
              else
                  t1 = MY_WTIME()
                  do i = 1, nrep
                    extent = MAX_COUNT*stride
                    recv_buffer(1:extent:stride) = &
                                 send_buffer(1:extent:stride)[partner]
                    if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                      nrep = i
                      exit
                    end if
                  end do
                  t2 = MY_WTIME()
              end if

              stats_buffer(num_stats) = &
                  dble(MAX_COUNT)*ELEM_SIZE*nrep/(1024*1024*(t2-t1))
          end if

          sync all

          if (ti == 1) then
              do i = 2, num_pairs
                  stats_buffer(num_stats) = stats_buffer(num_stats) + &
                                            stats_buffer(num_stats)[i]
              end do
              write (*, '(I20,I20,I20,F17.3, " MB/s")') &
                     MAX_COUNT, stride, nrep, stats_buffer(num_stats)/num_pairs
          end if

          num_stats = num_stats + 1
          stride = stride * 2
        end do
    end subroutine run_strided_get_bw_test


    subroutine run_strided_put_bidir_bw_test(strided)
        implicit none

        integer, intent(in) :: strided

        character (len=15) :: strided_label(3)
        double precision :: t1, t2
        integer :: ti, ni, nrep
        integer :: num_stats
        integer :: num_pairs
        integer, parameter :: MAX_COUNT = 32*1024
        integer, parameter :: MAX_BLKSIZE = BUFFER_SIZE
        integer, parameter :: MAX_STRIDE = MAX_BLKSIZE / MAX_COUNT
        integer :: stride,extent
        integer :: i

        ti = this_image()
        ni = num_images()
        num_pairs = numi / 2

        if (ti == 1) then
            strided_label(1) = "Target Strided"
            strided_label(2) = "Origin Strided"
            strided_label(3) = "Both Strided"

            write (*,'(//,"2-Way ",A," Put Bandwith: (",I0," pairs)")') &
                strided_label(strided), num_pairs
            write (*,'(A20, A20, A20, A20)') "count", "stride", "nrep", "bandwidth"
        end if

        num_stats = 1
        stride = 1
        do while (stride <= MAX_STRIDE)
          nrep = BW_NITER

          if (strided == TARGET_STRIDED) then
              t1 = MY_WTIME()
              do i = 1, nrep
                extent = MAX_COUNT*stride
                recv_buffer(1:extent:stride)[partner] = send_buffer(1:MAX_COUNT)
                if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                  nrep = i
                  exit
                end if
              end do
              t2 = MY_WTIME()
          else if (strided == ORIGIN_STRIDED) then
              t1 = MY_WTIME()
              do i = 1, nrep
                extent = MAX_COUNT*stride
                recv_buffer(1:MAX_COUNT)[partner] = send_buffer(1:extent:stride)
                if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                  nrep = i
                  exit
                end if
              end do
              t2 = MY_WTIME()
          else
              t1 = MY_WTIME()
              do i = 1, nrep
                extent = MAX_COUNT*stride
                recv_buffer(1:extent:stride)[partner] = &
                             send_buffer(1:extent:stride)
                if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                  nrep = i
                  exit
                end if
              end do
              t2 = MY_WTIME()
          end if

          stats_buffer(num_stats) = &
              dble(MAX_COUNT)*ELEM_SIZE*nrep/(1024*1024*(t2-t1))

          sync all

          if (ti == 1) then
              do i = 2, numi
                  stats_buffer(num_stats) = stats_buffer(num_stats) + &
                                            stats_buffer(num_stats)[i]
              end do
              write (*, '(I20,I20,I20,F17.3, " MB/s")') &
                     MAX_COUNT, stride, nrep, &
                     stats_buffer(num_stats)/numi
          end if

          num_stats = num_stats + 1
          stride = stride * 2
        end do
    end subroutine run_strided_put_bidir_bw_test

    subroutine run_strided_get_bidir_bw_test(strided)
        implicit none

        integer, intent(in) :: strided

        character (len=15) :: strided_label(3)
        double precision :: t1, t2
        integer :: ti, ni, nrep
        integer :: num_stats
        integer :: num_pairs
        integer, parameter :: MAX_COUNT = 32*1024
        integer, parameter :: MAX_BLKSIZE = BUFFER_SIZE
        integer, parameter :: MAX_STRIDE = MAX_BLKSIZE / MAX_COUNT
        integer :: stride,extent
        integer :: i

        ti = this_image()
        ni = num_images()
        num_pairs = numi / 2

        if (ti == 1) then
            strided_label(1) = "Target Strided"
            strided_label(2) = "Origin Strided"
            strided_label(3) = "Both Strided"

            write (*,'(//,"2-Way ",A, " Get Bandwith: (",I0," pairs)")') &
                strided_label(strided), num_pairs
            write (*,'(A20, A20, A20, A20)') "count", "stride", "nrep", "bandwidth"
        end if

        num_stats = 1
        stride = 1
        do while (stride <= MAX_STRIDE)
          nrep = BW_NITER

          if (strided == TARGET_STRIDED) then
              t1 = MY_WTIME()
              do i = 1, nrep
                extent = MAX_COUNT*stride
                recv_buffer(1:MAX_COUNT) = send_buffer(1:extent:stride)[partner]
                if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                  nrep = i
                  exit
                end if
              end do
              t2 = MY_WTIME()
          else if (strided == ORIGIN_STRIDED) then
              t1 = MY_WTIME()
              do i = 1, nrep
                extent = MAX_COUNT*stride
                recv_buffer(1:extent:stride) = send_buffer(1:MAX_COUNT)[partner]
                if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                  nrep = i
                  exit
                end if
              end do
              t2 = MY_WTIME()
          else
              t1 = MY_WTIME()
              do i = 1, nrep
                extent = MAX_COUNT*stride
                recv_buffer(1:extent:stride) = &
                             send_buffer(1:extent:stride)[partner]
                if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                  nrep = i
                  exit
                end if
              end do
              t2 = MY_WTIME()
          end if

          stats_buffer(num_stats) = &
              dble(MAX_COUNT)*ELEM_SIZE*nrep/(1024*1024*(t2-t1))

          sync all

          if (ti == 1) then
              do i = 2, numi
                  stats_buffer(num_stats) = stats_buffer(num_stats) + &
                                            stats_buffer(num_stats)[i]
              end do
              write (*, '(I20,I20,I20,F17.3, " MB/s")') &
                     MAX_COUNT, stride, nrep, &
                     stats_buffer(num_stats)/numi
          end if

          num_stats = num_stats + 1
          stride = stride * 2
        end do
    end subroutine run_strided_get_bidir_bw_test

#ifndef NO_LOCK_SUPPORT
    subroutine run_rand_strided_put_bw_test(strided)
        implicit none

        integer, intent(in) :: strided

        character (len=15) :: strided_label(3)
        double precision :: t1, t2
        integer :: ti, ni, nrep
        integer :: num_stats
        integer :: num_pairs
        integer, parameter :: MAX_COUNT = 32*1024
        integer, parameter :: MAX_BLKSIZE = BUFFER_SIZE
        integer, parameter :: MAX_STRIDE = MAX_BLKSIZE / MAX_COUNT
        integer :: stride,extent
        integer :: i
        integer :: rand_seed, rand_image
        real    :: real_rand_num

        ti = this_image()
        ni = num_images()
        num_pairs = numi / 2

        if (ti == 1) then
            strided_label(1) = "Target Strided"
            strided_label(2) = "Origin Strided"
            strided_label(3) = "Both Strided"

            write (*,'(//,A," Random Put Bandwith")') &
                strided_label(strided)
            write (*,'(A20, A20, A20, A20)') "count", "stride", "nrep", "bandwidth"
        end if

        num_stats = 1
        stride = 1
        do while (stride <= MAX_STRIDE)
          nrep = BW_NITER

          if (strided == TARGET_STRIDED) then
              t1 = MY_WTIME()
              do i = 1, nrep
                rand_seed = i*this_image()
                call random_seed(rand_seed)
                call random_number(real_rand_num)
                rand_image = INT(real_rand_num*num_images())+1
                extent = MAX_COUNT*stride
                lock (image_lock[rand_image])
                recv_buffer(1:extent:stride)[partner] = send_buffer(1:MAX_COUNT)
                unlock (image_lock[rand_image])
                if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                  nrep = i
                  exit
                end if
              end do
              t2 = MY_WTIME()
          else if (strided == ORIGIN_STRIDED) then
              t1 = MY_WTIME()
              do i = 1, nrep
                rand_seed = i*this_image()
                call random_seed(rand_seed)
                call random_number(real_rand_num)
                rand_image = INT(real_rand_num*num_images())+1
                extent = MAX_COUNT*stride
                lock (image_lock[rand_image])
                recv_buffer(1:MAX_COUNT)[partner] = send_buffer(1:extent:stride)
                unlock (image_lock[rand_image])
                if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                  nrep = i
                  exit
                end if
              end do
              t2 = MY_WTIME()
          else
              t1 = MY_WTIME()
              do i = 1, nrep
                rand_seed = i*this_image()
                call random_seed(rand_seed)
                call random_number(real_rand_num)
                rand_image = INT(real_rand_num*num_images())+1
                extent = MAX_COUNT*stride
                lock (image_lock[rand_image])
                recv_buffer(1:extent:stride)[partner] = &
                             send_buffer(1:extent:stride)
                unlock (image_lock[rand_image])
                if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                  nrep = i
                  exit
                end if
              end do
              t2 = MY_WTIME()
          end if

          stats_buffer(num_stats) = &
              dble(MAX_COUNT)*ELEM_SIZE*nrep/(1024*1024*(t2-t1))

          sync all

          if (ti == 1) then
              do i = 2, num_images()
                  stats_buffer(num_stats) = stats_buffer(num_stats) + &
                                            stats_buffer(num_stats)[i]
              end do
              write (*, '(I20,I20,I20,F17.3, " MB/s")') &
                     MAX_COUNT, stride, nrep, &
                     stats_buffer(num_stats)/num_images()
          end if

          num_stats = num_stats + 1
          stride = stride * 2
        end do
    end subroutine run_rand_strided_put_bw_test

    subroutine run_rand_strided_get_bw_test(strided)
        implicit none

        integer, intent(in) :: strided

        character (len=15) :: strided_label(3)
        double precision :: t1, t2
        integer :: ti, ni, nrep
        integer :: num_stats
        integer :: num_pairs
        integer, parameter :: MAX_COUNT = 32*1024
        integer, parameter :: MAX_BLKSIZE = BUFFER_SIZE
        integer, parameter :: MAX_STRIDE = MAX_BLKSIZE / MAX_COUNT
        integer :: stride,extent
        integer :: i
        integer :: rand_seed, rand_image
        real    :: real_rand_num

        ti = this_image()
        ni = num_images()
        num_pairs = numi / 2

        if (ti == 1) then
            strided_label(1) = "Target Strided"
            strided_label(2) = "Origin Strided"
            strided_label(3) = "Both Strided"

            write (*,'(//,A, " Random Get Bandwith ")') &
                strided_label(strided)
            write (*,'(A20, A20, A20, A20)') "count", "stride", "nrep", "bandwidth"
        end if

        num_stats = 1
        stride = 1
        do while (stride <= MAX_STRIDE)
          nrep = BW_NITER

          if (strided == TARGET_STRIDED) then
              t1 = MY_WTIME()
              do i = 1, nrep
                rand_seed = i*this_image()
                call random_seed(rand_seed)
                call random_number(real_rand_num)
                rand_image = INT(real_rand_num*num_images())+1
                extent = MAX_COUNT*stride
                lock (image_lock[rand_image])
                recv_buffer(1:MAX_COUNT) = send_buffer(1:extent:stride)[partner]
                unlock (image_lock[rand_image])
                if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                  nrep = i
                  exit
                end if
              end do
              t2 = MY_WTIME()
          else if (strided == ORIGIN_STRIDED) then
              t1 = MY_WTIME()
              do i = 1, nrep
                extent = MAX_COUNT*stride
                recv_buffer(1:extent:stride) = send_buffer(1:MAX_COUNT)[partner]
                if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                  nrep = i
                  exit
                end if
              end do
              t2 = MY_WTIME()
          else
              t1 = MY_WTIME()
              do i = 1, nrep
                extent = MAX_COUNT*stride
                recv_buffer(1:extent:stride) = &
                             send_buffer(1:extent:stride)[partner]
                if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
                  nrep = i
                  exit
                end if
              end do
              t2 = MY_WTIME()
          end if

          stats_buffer(num_stats) = &
              dble(MAX_COUNT)*ELEM_SIZE*nrep/(1024*1024*(t2-t1))

          sync all

          if (ti == 1) then
              do i = 2, num_images()
                  stats_buffer(num_stats) = stats_buffer(num_stats) + &
                                            stats_buffer(num_stats)[i]
              end do
              write (*, '(I20,I20,I20,F17.3, " MB/s")') &
                     MAX_COUNT, stride, nrep, &
                     stats_buffer(num_stats)/num_images()
          end if

          num_stats = num_stats + 1
          stride = stride * 2
        end do
    end subroutine run_rand_strided_get_bw_test
#endif

#endif

#ifdef REDUCE_TESTS
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !                   REDUCTION TESTS
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef CO_REDUCTIONS_SUPPORT
    subroutine run_reduce_test(separate_target)
        implicit none
        logical :: separate_target

        double precision :: t1, t2
        integer :: ti, ni, nrep
        integer :: num_stats
        integer :: blksize
        integer :: i

        ti = this_image()
        ni = num_images()

        if (ti == 1) then
            if (separate_target) then
                write (*,'(//,"Reduction: (src != target)")')
            else
                write (*,'(//,"Reduction: (src == target)")')
            end if
            write (*,'(A20, A20, A20)') "blksize", "nrep", "latency"
        end if

        num_stats = 1
        blksize = 1
        do while (blksize <= BUFFER_SIZE)
          nrep = RED_NITER

          t1 = MY_WTIME()
          do i = 1, nrep
            !recv_buffer(1:blksize)[partner] = send_buffer(1:blksize)
            if (separate_target) then
                call co_sum(send_buffer(1:blksize), recv_buffer(1:blksize));
            else
                call co_sum(send_buffer(1:blksize), send_buffer(1:blksize));
            end if
            if (mod(i,10) == 0 .and. (MY_WTIME()-t1) > TIMEOUT) then
              nrep = i
              exit
            end if
          end do
          t2 = MY_WTIME()

          stats_buffer(num_stats) = 1000000*(t2-t1)/(RED_NITER)

          sync all

          if (ti == 1) then
              do i = 2, num_images()
                  stats_buffer(num_stats) = stats_buffer(num_stats) + &
                                            stats_buffer(num_stats)[i]
              end do
              write (*, '(I20,I20,F17.3, " us")') &
                     blksize, nrep, stats_buffer(num_stats)/num_images()
          end if

          num_stats = num_stats + 1
          blksize = blksize * 2
        end do

    end subroutine run_reduce_test
#endif

#endif

end module caf_microbenchmarks

program main
    use caf_microbenchmarks

    implicit none

    integer :: PO[*], nargs
    character(len=20) :: arg
    integer :: i

    PO = 0

    if (this_image() == 1) then
        nargs = command_argument_count()
        if (nargs /= 0) then
            call get_command_argument(1, arg)
            read (arg,'(i20)') PO
            do i = 2, num_images()
              PO[i] = PO
            end do
        end if
    end if

    sync all

    if (mod(num_images(), 2) /= 0) then
        error stop "use an even number of images"
    else if (PO > 0 .and. mod(num_images(), 2*PO) /= 0) then
        error stop "number of images must be a multiple of 2 * parter offset"
    end if

    numi = num_images()
    if (PO == 0) then
        partner = 1 + mod(this_image()-1+numi/2, numi)
    else
        if (mod(this_image()-1,2*PO) < PO) then
            partner = this_image() + PO
        else
            partner = this_image() - PO
        end if
    end if

    allocate ( stats_buffer(NUM_STATS)[*] )

#ifdef LATENCY_TESTS
    call run_sync_latency_test(BARRIER)
    call run_sync_latency_test(P2P)
    call run_putget_latency_test()
    call run_putput_latency_test(BARRIER)
    call run_putput_latency_test(P2P)
    call run_getget_latency_test(BARRIER)
    call run_getget_latency_test(P2P)
#endif

#ifdef BW_TESTS
    call run_put_bw_test()
    call run_get_bw_test()
    call run_put_bw_bidir_test()
    call run_get_bw_bidir_test()
#ifndef NO_LOCK_SUPPORT
    call run_rand_put_bw_test()
    call run_rand_get_bw_test()
#endif
#endif

#ifdef STRIDED_BW_TESTS
    call run_strided_put_bw_test(TARGET_STRIDED)
    call run_strided_put_bw_test(ORIGIN_STRIDED)
    call run_strided_put_bw_test(BOTH_STRIDED)

    call run_strided_get_bw_test(TARGET_STRIDED)
    call run_strided_get_bw_test(ORIGIN_STRIDED)
    call run_strided_get_bw_test(BOTH_STRIDED)

    call run_strided_put_bidir_bw_test(TARGET_STRIDED)
    call run_strided_put_bidir_bw_test(ORIGIN_STRIDED)
    call run_strided_put_bidir_bw_test(BOTH_STRIDED)

    call run_strided_get_bidir_bw_test(TARGET_STRIDED)
    call run_strided_get_bidir_bw_test(ORIGIN_STRIDED)
    call run_strided_get_bidir_bw_test(BOTH_STRIDED)

#ifndef NO_LOCK_SUPPORT
    call run_rand_strided_put_bw_test(TARGET_STRIDED)
    call run_rand_strided_put_bw_test(ORIGIN_STRIDED)
    call run_rand_strided_put_bw_test(BOTH_STRIDED)

    call run_rand_strided_get_bw_test(TARGET_STRIDED)
    call run_rand_strided_get_bw_test(ORIGIN_STRIDED)
    call run_rand_strided_get_bw_test(BOTH_STRIDED)
#endif
#endif

#ifdef REDUCE_TESTS
#ifdef CO_REDUCTIONS_SUPPORT
    call run_reduce_test(.false.)
    call run_reduce_test(.true.)
#endif
#endif
end program
