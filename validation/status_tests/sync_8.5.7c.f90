! SYNC IMAGES(*) with the STAT=STAT_STOPPED_IMAGE specifier

      program errmsg_stat
        use, intrinsic:: iso_fortran_env

        implicit none

        integer :: stat_var, me

        me = this_image()

        if (num_images() == 1) then
           print *, "Config error: NPROCS should be greater than 1"
           ERROR STOP 1
        end if

        if (me /= 1 ) then
           call sleep(SLEEP)
           sync images(*, STAT=stat_var)
           if ( stat_var /= STAT_STOPPED_IMAGE) then
               print *, "Error:stat_var /= STAT_STOPPED_IMAGE: ", me
               ERROR STOP 1
           end if
        end if

        print *, "image", me, "stopping"
      end program

