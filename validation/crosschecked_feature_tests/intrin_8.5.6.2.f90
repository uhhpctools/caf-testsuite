! Testing for LOCK & UNLOCK without STAT specifier

      program lock_unlock
      use cross_test

         USE, INTRINSIC :: ISO_FORTRAN_ENV
         type(LOCK_TYPE) :: lock_var [*]
         integer :: num[*]
         integer :: rank
         integer,parameter :: MAX=1000
         integer :: dummy(MAX)


         rank = this_image()

        if(NPROCS .gt. 1) then
          do i = 1,NITER
              num = 0
              sync all

#ifndef CROSS_
              ! testing whether img 1 can LOCK a reference to
              ! non-cosubscripted local coarray
              if(MOD(i,2) == 0 .and. rank == 1 ) then
                 LOCK(lock_var)
              else
                 LOCK (lock_var[1])
              end if
#endif

              do j = 1 , MAX
                num[1] = num[1] + 1
                dummy(j)=dummy(j)+1
              end do

#ifndef CROSS_
              ! testing whether img 1 can UNLOCK a reference to
              ! non-cosubscripted local coarray
              if(MOD(i,2) == 0 .and. rank == 1 ) then
                 UNLOCK(lock_var)
              else
                 UNLOCK (lock_var[1])
              end if
#endif
             sync all
             if (rank == 1 .and. num /=(1000*NPROCS)) then
                cross_err = cross_err + 1
             end if
             sync all
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

