! SYNC IMAGES(*) .NEQ. SYNC ALL - other images must not wait for each other

PROGRAM item12_c
    use cross_test

    IMPLICIT NONE

    INTEGER :: total_images, rank, i
    INTEGER :: arr (1)
    INTEGER :: num[*]

    rank=this_image()
    total_images=num_images()
    arr(1)=2

    cross_err = 0

    if (total_images .ge. 3) then

      do i = 1 , NITER
          num = 0
          sync all
          if(rank == 2)  then
#ifndef CROSS_
               sync images(*)
#else
               sync all
#endif
          else if(rank == 3) then
               call sleep(SLEEP)
               num = rank
#ifndef CROSS_
               sync images(arr)
#else
               sync all
#endif
              print *, "3 leaving sync"
          else
#ifndef CROSS_
               sync images(arr)
#else
               sync all
#endif
              print * , "image", rank , "already reading val", num[3]
               if (rank == 1 .and. num[3] == 3 ) then
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
          print *, "Config Err: NPROCS shoud be >= 3"
          call EXIT (1)
        end if


END PROGRAM
