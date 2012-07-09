! SYNC ALL without the STAT= specifier

      program item12_1
      use cross_test

        implicit none

        integer :: num[*]
        integer :: i, rank
        if(NPROCS .gt. 1) then

        rank=this_image()

#ifndef CROSS_
        sync all
#endif

        do i = 1,NITER
              num = 0
#ifndef CROSS_
              sync all
#endif
              if (rank == 2) then
                num = 0
                call sleep(SLEEP)
                num = i*rank
#ifndef CROSS_
                sync all
#endif

              else

#ifndef CROSS_
                sync all
#endif
                if (rank == 1 .and. num[2]/=i*2) then
                    cross_err = cross_err + 1
                end if
              end if
#ifndef CROSS_
             sync all
#endif
        end do
#ifndef CROSS_
           call calc_ori()
#else
           call calc(NITER)
#endif

      else
          print *, "Config Err: NPROCS shoud be > 1"
          call EXIT (1)
        end if
      end program

