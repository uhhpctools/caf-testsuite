!Atomic subroutines

       program main
        use cross_test
        use, intrinsic:: iso_fortran_env

        integer(atomic_int_kind) :: i_obj[*]
        logical(atomic_logical_kind) :: l_obj[*] = .true.
        integer :: sz, rank,tot_NITER=NITER!*10000
        integer :: i,k,j
        logical :: l_val
        integer(atomic_int_kind) :: num,i_val

        sz = NPROCS
        rank = this_image()
        num = (int(256,atomic_int_kind))**(MOD(rank,sizeof(num)))

        if (NPROCS .lt. 2) then
          print *, "Config Error: Number of images must be >= 2"
          call EXIT(1)
        end if

        do k = 1,tot_NITER
            i_val=-1
            i_obj = -1
            sync all
               if (rank == 1) then
#ifndef CROSS_
                    do while (i_val == -1)
                        call atomic_ref(i_val, i_obj)
                    end do
#else
                    do while (i_val == -1)
                        i_val = i_obj
                    end do
#endif
                    do while ( MOD(i_val,int(2,atomic_int_kind)) == 0)
                      i_val = i_val / 2
                    end do

                    if ( i_val /= 1) then
                        print *, "Error for atomic_int_kind"
                        cross_err = cross_err + 1
                    end if

               else
                  !Each image defines i_obj[1] multiple times
                  !This increases the chance of an incorrect
                  !implementation of atomics to mess up
#ifndef CROSS_
                     call atomic_define(i_obj[1],num)
#else
                     do j = 1,NITER
                         i_obj[1]=num
                     end do
#endif
               end if

        end do


        ! The source of the code below is
        ! ISO/IEC JTC1/SC22/WG5 N1824
        ! This is a very good test for atomic_logical_kind coarrays

        if (rank == 1) then
        sync memory
        call atomic_define(l_obj[2],.false.)
        ! Has the effect of l_obj[q]=.false.
        else if (rank == 2) then
        l_val = .true.
        ! Spin until l_val is false
        do while (l_val)
        call atomic_ref(l_val,l_obj)
        ! Has the effect of l_val=l_obj
        end do
        sync memory
        end if


           sync all

#ifndef CROSS_
           call calc_ori()
#else
           call calc(tot_NITER)
#endif

      end program

