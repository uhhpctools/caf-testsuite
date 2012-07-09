!SYNC IMAGES(arr) must sync with all images listed in arr()

PROGRAM item12_c
    use cross_test

       IMPLICIT NONE

       INTEGER :: total_images, rank, i
       INTEGER :: arr(1)
       INTEGER :: num[*]
       if(NPROCS .gt. 1) then

        rank=this_image()
        arr(1)=2

        sync all

        do i = 1,NITER
              num = 0
              sync all
              if(rank == 2)  then
                  num = 0
                  call sleep(SLEEP)
                  num = i*rank
#ifndef CROSS_
                  sync images(*)
#endif
              else
#ifndef CROSS_
                  sync images(arr)
#endif
                  if (rank == 1 .and. num[2] /= i*2) then
                    cross_err = cross_err + 1
                  end if
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

