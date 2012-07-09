      module custom_coreduce
        private

        public :: coreduce_r8, coreduce_c8
        public :: CAF_SUM, CAF_MAX, CAF_MIN

        integer, parameter :: CAF_SUM = 1
        integer, parameter :: CAF_MAX = 2
        integer, parameter :: CAF_MIN = 3

        integer, parameter :: WORK_BUF_SIZE=256

        contains


           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           ! Reduction for 8-byte Real Arrays
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           subroutine coreduce_r8_to_image( op, source, result_image,  &
                                           work_buf, sz)
               implicit none
               integer :: op
               integer :: sz
               real (kind=8) :: source(sz)
               real (kind=8) :: work_buf(sz)[*]
               integer :: result_image

               integer :: me, p, step

               me = this_image()
               p = num_images()

               work_buf(1:sz) = source(1:sz)

               step = 1
               do while (step < p)
                 if ( mod(me-1,2*step) == 0) then
                   if ((me+step) <= p) then
                       sync images (me+step)
                       source(:) = work_buf(:)[me+step]
                       if (op == CAF_SUM) then
                           work_buf(1:sz) = work_buf(1:sz)+source(1:sz)
                       else if (op == CAF_MAX) then
                           work_buf(1:sz) = max(work_buf(1:sz), source(1:sz))
                       else if (op == CAF_MIN) then
                           work_buf(1:sz) = min(work_buf(1:sz), source(1:sz))
                       end if
                   end if
                 else if (mod(me-1,2*step) == step) then
                   if ((me-step) >= 1) then
                       sync images (me-step)
                   end if
                 end if
                 step = step * 2
               end do


               if (me == result_image) then
                 if (me /= 1) then
                     sync images (1)
                     sync images (1)
                 end if
                 source(1:sz) = work_buf(1:sz)
               else if (me == 1) then
                 sync images (result_image)
                 work_buf(1:sz)[result_image] = work_buf(1:sz)
                 sync images (result_image)
               end if

           end subroutine coreduce_r8_to_image

            subroutine coreduce_r8_to_all(op, B, work, N)
                implicit none
                integer                 :: op
                integer                 :: N
                real (kind=8), intent(inout)  :: B(N)
                real (kind=8)        :: work(N)[*]

                integer :: p, q, r, me, partner
                integer :: i, step, log2_q

                me = this_image()
                p = num_images()

                !-------------------------------------------------
                ! Find greatest power of 2 less than p, q
                !-------------------------------------------------
                q = 1
                log2_q = 0
                do while (2*q <= p)
                  q = 2*q
                  log2_q = log2_q + 1
                end do

                !-------------------------------------------------
                ! r is the number of remaining processes, after q
                !-------------------------------------------------
                r = p - q

                !-------------------------------------------------
                ! last r process put values to first r processes
                !-------------------------------------------------
                if (me > q) then
                    partner = me - q
                    work(1:N)[partner] = B(1:N)
                    sync images (partner)
                else if (me <= r) then
                    partner = me + q
                    sync images (partner)
                    if (op == CAF_SUM) then
                        B(1:N) = B(1:N) + work(1:N)
                    else if (op == CAF_MAX) then
                        B(1:N) = max(B(1:N), work(1:N))
                    else if (op == CAF_MIN) then
                        B(1:N) = min(B(1:N), work(1:N))
                    end if
                end if

                !---------------------------------------------------
                ! first q processes do recursive-doubling algorithm
                !---------------------------------------------------
                if (me <= q) then
                    step = 1
                    do while (step < q)
                      if (mod(me-1,2*step) < step) then
                          partner = me + step
                          sync images (partner)
                          work(1:N)[partner] = B(1:N)
                          sync images (partner)
                          if (op == CAF_SUM) then
                              B(1:N) = B(1:N) + work(1:N)
                          else if (op == CAF_MAX) then
                              B(1:N) = max(B(1:N), work(1:N))
                          else if (op == CAF_MIN) then
                              B(1:N) = min(B(1:N), work(1:N))
                          end if
                      else
                          partner = me - step
                          sync images (partner)
                          work(1:N)[partner] = B(1:N)
                          sync images (partner)
                          if (op == CAF_SUM) then
                              B(1:N) = B(1:N) + work(1:N)
                          else if (op == CAF_MAX) then
                              B(1:N) = max(B(1:N), work(1:N))
                          else if (op == CAF_MIN) then
                              B(1:N) = min(B(1:N), work(1:N))
                          end if
                      end if
                      step = step*2
                    end do
                end if

                !-------------------------------------------------
                ! first r process put values to last r processes
                !-------------------------------------------------
                if (me <= r) then
                    partner = me + q
                    work(1:N)[partner] = B(1:N)
                    sync images (partner)
                else if (me > q) then
                    partner = me - q
                    sync images (partner)
                    B(1:N) = work(1:N)
                end if

            end subroutine coreduce_r8_to_all

            subroutine coreduce_r8( op, source, result, result_image )
                implicit none
                integer :: op
                real (kind=8) :: source(:)
                real (kind=8), optional, intent(out) :: result(:)
                integer, optional, intent(in)  :: result_image

                real (kind=8), save :: work_buf(WORK_BUF_SIZE)[*]
                real (kind=8), allocatable :: alt_work_buf(:)[:]
                real (kind=8), allocatable :: contig_src(:)

                if (size(source) > WORK_BUF_SIZE) then
                    allocate( alt_work_buf(size(source))[*] )
                end if

                if ( present(result) .and. present(result_image) ) then
                    !------------------------------------------------------
                    ! reduce to result on given image
                    !------------------------------------------------------

#ifndef IS_CONTIGUOUS_NOTAVAIL
                    if (is_contiguous(result)) then
#else
                    if (.false.) then
#endif
                        result(:) = source(:)
                        if (size(source) > WORK_BUF_SIZE) then
                            call coreduce_r8_to_image(op, result, &
                                                     result_image, &
                                                     alt_work_buf, &
                                                     size(result))
                        else
                            call coreduce_r8_to_image(op, result, &
                                                     result_image, &
                                                     work_buf,     &
                                                     size(result))
                        end if

                    else
                        allocate( contig_src(size(result, 1)) )
                        contig_src(:) = source(:)
                        if (size(source) > WORK_BUF_SIZE) then
                            call coreduce_r8_to_image(op, contig_src, &
                                                     result_image,   &
                                                     alt_work_buf,   &
                                                     size(contig_src))
                        else
                            call coreduce_r8_to_image(op, contig_src, &
                                                     result_image,   &
                                                     work_buf,       &
                                                     size(contig_src))
                        end if
                        if (this_image() == result_image) then
                            result(:) = contig_src(:)
                        end if

                        deallocate( contig_src )
                    end if

                else if ( present(result_image) ) then
                    !------------------------------------------------------
                    ! reduce to source on given image
                    !------------------------------------------------------

#ifndef IS_CONTIGUOUS_NOTAVAIL
                    if (is_contiguous(source)) then
#else
                    if (.false.) then
#endif
                        if (size(source) > WORK_BUF_SIZE) then
                            call coreduce_r8_to_image(op, source,    &
                                                     result_image,  &
                                                     alt_work_buf,  &
                                                     size(source))
                        else
                            call coreduce_r8_to_image(op, source,    &
                                                     result_image,  &
                                                     work_buf,      &
                                                     size(source))
                        end if
                    else
                        allocate( contig_src(size(source, 1)) )
                        contig_src(:) = source(:)
                        if (size(source) > WORK_BUF_SIZE) then
                            call coreduce_r8_to_image(op, contig_src, &
                                                     result_image,   &
                                                     alt_work_buf,   &
                                                     size(contig_src))
                        else
                            call coreduce_r8_to_image(op, contig_src, &
                                                     result_image,   &
                                                     work_buf,       &
                                                     size(contig_src))
                        end if
                        if (this_image() == result_image) then
                            source(:) = contig_src(:)
                        end if
                        deallocate( contig_src )
                    end if

                else if ( present(result) ) then
                    !------------------------------------------------------
                    ! reduce to result on all images
                    !------------------------------------------------------


#ifndef IS_CONTIGUOUS_NOTAVAIL
                    if (is_contiguous(result)) then
#else
                    if (.false.) then
#endif
                        result(:) = source(:)
                        if (size(source) > WORK_BUF_SIZE) then
                            call coreduce_r8_to_all(op, result,    &
                                                   alt_work_buf,  &
                                                   size(result))
                        else
                            call coreduce_r8_to_all(op, result,  &
                                                   work_buf,    &
                                                   size(result))
                        end if
                    else
                        allocate( contig_src(size(result, 1)) )
                        contig_src(:) = source(:)
                        if (size(source) > WORK_BUF_SIZE) then
                            call coreduce_r8_to_all(op, contig_src, &
                                                   alt_work_buf,   &
                                                   size(contig_src))
                        else
                            call coreduce_r8_to_all(op, contig_src, &
                                                   work_buf,       &
                                                   size(contig_src))
                        end if
                        result(:) = contig_src(:)
                        deallocate( contig_src )
                    end if

                else
                    !------------------------------------------------------
                    ! reduce to source on all images
                    !------------------------------------------------------

#ifndef IS_CONTIGUOUS_NOTAVAIL
                    if (is_contiguous(source)) then
#else
                    if (.false.) then
#endif
                        if (size(source) > WORK_BUF_SIZE) then
                            call coreduce_r8_to_all(op, source,      &
                                                   alt_work_buf,    &
                                                   size(source))
                        else
                            call coreduce_r8_to_all(op, source,      &
                                                   work_buf,        &
                                                   size(source))
                        end if
                    else
                        allocate( contig_src(size(source, 1)) )
                        contig_src(:) = source(:)
                        if (size(source) > WORK_BUF_SIZE) then
                            call coreduce_r8_to_all(op, contig_src, &
                                                   alt_work_buf,   &
                                                   size(contig_src))
                        else
                            call coreduce_r8_to_all(op, contig_src, &
                                                   work_buf,       &
                                                   size(contig_src))
                        end if
                        source(:) = contig_src(:)
                        deallocate( contig_src )
                    end if

                end if

                if (size(source) > WORK_BUF_SIZE) then
                    deallocate( alt_work_buf )
                end if

            end subroutine coreduce_r8

           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           ! Reduction for Double Precision Complex Arrays
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           subroutine coreduce_c8_to_image( op, source, result_image,  &
                                           work_buf, sz)
               implicit none
               integer :: op
               integer :: sz
               double complex :: source(sz)
               double complex :: work_buf(sz)[*]
               integer :: result_image

               integer :: me, p, step

               me = this_image()
               p = num_images()

               work_buf(1:sz) = source(1:sz)

               step = 1
               do while (step < p)
                 if ( mod(me-1,2*step) == 0) then
                   if ((me+step) <= p) then
                       sync images (me+step)
                       source(:) = work_buf(:)[me+step]
                       if (op == CAF_SUM) then
                           work_buf(1:sz) = work_buf(1:sz)+source(1:sz)
                       end if
                   end if
                 else if (mod(me-1,2*step) == step) then
                   if ((me-step) >= 1) then
                       sync images (me-step)
                   end if
                 end if
                 step = step * 2
               end do


               if (me == result_image) then
                 if (me /= 1) then
                     sync images (1)
                     sync images (1)
                 end if
                 source(1:sz) = work_buf(1:sz)
               else if (me == 1) then
                 sync images (result_image)
                 work_buf(1:sz)[result_image] = work_buf(1:sz)
                 sync images (result_image)
               end if

           end subroutine coreduce_c8_to_image

            subroutine coreduce_c8_to_all(op, B, work, N)
                implicit none
                integer                 :: op
                integer                 :: N
                double complex, intent(inout)  :: B(N)
                double complex        :: work(N)[*]

                integer :: p, q, r, me, partner
                integer :: i, step, log2_q

                me = this_image()
                p = num_images()

                !-------------------------------------------------
                ! Find greatest power of 2 less than p, q
                !-------------------------------------------------
                q = 1
                log2_q = 0
                do while (2*q <= p)
                  q = 2*q
                  log2_q = log2_q + 1
                end do

                !-------------------------------------------------
                ! r is the number of remaining processes, after q
                !-------------------------------------------------
                r = p - q

                !-------------------------------------------------
                ! last r process put values to first r processes
                !-------------------------------------------------
                if (me > q) then
                    partner = me - q
                    work(1:N)[partner] = B(1:N)
                    sync images (partner)
                else if (me <= r) then
                    partner = me + q
                    sync images (partner)
                    if (op == CAF_SUM) then
                        B(1:N) = B(1:N) + work(1:N)
                    end if
                end if

                !---------------------------------------------------
                ! first q processes do recursive-doubling algorithm
                !---------------------------------------------------
                if (me <= q) then
                    step = 1
                    do while (step < q)
                      if (mod(me-1,2*step) < step) then
                          partner = me + step
                          sync images (partner)
                          work(1:N)[partner] = B(1:N)
                          sync images (partner)
                          if (op == CAF_SUM) then
                              B(1:N) = B(1:N) + work(1:N)
                          end if
                      else
                          partner = me - step
                          sync images (partner)
                          work(1:N)[partner] = B(1:N)
                          sync images (partner)
                          if (op == CAF_SUM) then
                              B(1:N) = B(1:N) + work(1:N)
                          end if
                      end if
                      step = step*2
                    end do
                end if

                !-------------------------------------------------
                ! first r process put values to last r processes
                !-------------------------------------------------
                if (me <= r) then
                    partner = me + q
                    work(1:N)[partner] = B(1:N)
                    sync images (partner)
                else if (me > q) then
                    partner = me - q
                    sync images (partner)
                    B(1:N) = work(1:N)
                end if

            end subroutine coreduce_c8_to_all

            subroutine coreduce_c8( op, source, result, result_image )
                implicit none
                integer :: op
                double complex :: source(:)
                double complex, optional, intent(out) :: result(:)
                integer, optional, intent(in)  :: result_image

                double complex, save :: work_buf(WORK_BUF_SIZE)[*]
                double complex, allocatable :: alt_work_buf(:)[:]
                double complex, allocatable :: contig_src(:)

                if (size(source) > WORK_BUF_SIZE) then
                    allocate( alt_work_buf(size(source))[*] )
                end if

                if ( present(result) .and. present(result_image) ) then
                    !------------------------------------------------------
                    ! reduce to result on given image
                    !------------------------------------------------------

#ifndef IS_CONTIGUOUS_NOTAVAIL
                    if (is_contiguous(result)) then
#else
                    if (.false.) then
#endif
                        result(:) = source(:)
                        if (size(source) > WORK_BUF_SIZE) then
                            call coreduce_c8_to_image(op, result, &
                                                     result_image, &
                                                     alt_work_buf, &
                                                     size(result))
                        else
                            call coreduce_c8_to_image(op, result, &
                                                     result_image, &
                                                     work_buf,     &
                                                     size(result))
                        end if

                    else
                        allocate( contig_src(size(result, 1)) )
                        contig_src(:) = source(:)
                        if (size(source) > WORK_BUF_SIZE) then
                            call coreduce_c8_to_image(op, contig_src, &
                                                     result_image,   &
                                                     alt_work_buf,   &
                                                     size(contig_src))
                        else
                            call coreduce_c8_to_image(op, contig_src, &
                                                     result_image,   &
                                                     work_buf,       &
                                                     size(contig_src))
                        end if
                        if (this_image() == result_image) then
                            result(:) = contig_src(:)
                        end if

                        deallocate( contig_src )
                    end if

                else if ( present(result_image) ) then
                    !------------------------------------------------------
                    ! reduce to source on given image
                    !------------------------------------------------------

#ifndef IS_CONTIGUOUS_NOTAVAIL
                    if (is_contiguous(source)) then
#else
                    if (.false.) then
#endif
                        if (size(source) > WORK_BUF_SIZE) then
                            call coreduce_c8_to_image(op, source,    &
                                                     result_image,  &
                                                     alt_work_buf,  &
                                                     size(source))
                        else
                            call coreduce_c8_to_image(op, source,    &
                                                     result_image,  &
                                                     work_buf,      &
                                                     size(source))
                        end if
                    else
                        allocate( contig_src(size(source, 1)) )
                        contig_src(:) = source(:)
                        if (size(source) > WORK_BUF_SIZE) then
                            call coreduce_c8_to_image(op, contig_src, &
                                                     result_image,   &
                                                     alt_work_buf,   &
                                                     size(contig_src))
                        else
                            call coreduce_c8_to_image(op, contig_src, &
                                                     result_image,   &
                                                     work_buf,       &
                                                     size(contig_src))
                        end if
                        if (this_image() == result_image) then
                            source(:) = contig_src(:)
                        end if
                        deallocate( contig_src )
                    end if

                else if ( present(result) ) then
                    !------------------------------------------------------
                    ! reduce to result on all images
                    !------------------------------------------------------


#ifndef IS_CONTIGUOUS_NOTAVAIL
                    if (is_contiguous(result)) then
#else
                    if (.false.) then
#endif
                        result(:) = source(:)
                        if (size(source) > WORK_BUF_SIZE) then
                            call coreduce_c8_to_all(op, result,    &
                                                   alt_work_buf,  &
                                                   size(result))
                        else
                            call coreduce_c8_to_all(op, result,  &
                                                   work_buf,    &
                                                   size(result))
                        end if
                    else
                        allocate( contig_src(size(result, 1)) )
                        contig_src(:) = source(:)
                        if (size(source) > WORK_BUF_SIZE) then
                            call coreduce_c8_to_all(op, contig_src, &
                                                   alt_work_buf,   &
                                                   size(contig_src))
                        else
                            call coreduce_c8_to_all(op, contig_src, &
                                                   work_buf,       &
                                                   size(contig_src))
                        end if
                        result(:) = contig_src(:)
                        deallocate( contig_src )
                    end if

                else
                    !------------------------------------------------------
                    ! reduce to source on all images
                    !------------------------------------------------------

#ifndef IS_CONTIGUOUS_NOTAVAIL
                    if (is_contiguous(source)) then
#else
                    if (.false.) then
#endif
                        if (size(source) > WORK_BUF_SIZE) then
                            call coreduce_c8_to_all(op, source,      &
                                                   alt_work_buf,    &
                                                   size(source))
                        else
                            call coreduce_c8_to_all(op, source,      &
                                                   work_buf,        &
                                                   size(source))
                        end if
                    else
                        allocate( contig_src(size(source, 1)) )
                        contig_src(:) = source(:)
                        if (size(source) > WORK_BUF_SIZE) then
                            call coreduce_c8_to_all(op, contig_src, &
                                                   alt_work_buf,   &
                                                   size(contig_src))
                        else
                            call coreduce_c8_to_all(op, contig_src, &
                                                   work_buf,       &
                                                   size(contig_src))
                        end if
                        source(:) = contig_src(:)
                        deallocate( contig_src )
                    end if

                end if

                if (size(source) > WORK_BUF_SIZE) then
                    deallocate( alt_work_buf )
                end if

            end subroutine coreduce_c8


      end module
