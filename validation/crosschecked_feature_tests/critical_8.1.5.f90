! CRITICAL sections

         program critical
         use  cross_test

         integer :: num[*], me, i,j
         integer,parameter :: M=100000
         integer :: dummy(M)


         me = this_image()
         if(num_images() .gt. 1) then

            num = 0
           do i = 1,NITER
                  num = 0
                  sync all
#ifndef CROSS_
              critical
#endif
              do j = 1 , M
                  num[1] = num[1] + 1
              end do
#ifndef CROSS_
             end critical
#endif
             sync all

             if (me == 1 .and. num /=(M*NPROCS)) then
                cross_err = cross_err + 1
                print *,"Error: num = ", num
             end if
             sync all
           end do
#ifndef CROSS_
           print * , "call before", me, "err=", cross_err
           call calc_ori()
           print *, "call after", me
#else
           call calc(NITER)
#endif

        else
          print *, "Config Err: NPROCS shoud be > 1"
          call EXIT (1)
        end if

        print *, "num", num[1]
        print *, "cross_err", cross_err

        end program

