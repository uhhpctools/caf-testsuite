! ERROR STOP statement should stop other images as soon as it is practicable

    PROGRAM errorstop_test
    use cross_test

       IMPLICIT NONE

       integer :: rank, size,i, temp

       rank = this_image()

       sync all

       if (rank == 1) then
#ifndef CROSS_
         error stop 10
#endif
       else

         ! The following loop is added just to get some computational overhead
         ! and to make is possible for ERROR termination
         do i = 1 , NITER
            call sleep(SLEEP)
            print *, i, " "
            temp = temp + 100
         end do
                 print *, "out of the loop"
         do i = 1 , NITER
            call sleep(SLEEP)
            print *, i, " "
            temp = temp + 100
         end do

       end if

    END PROGRAM
