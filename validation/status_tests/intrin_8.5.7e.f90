! Testing for LOCK & UNLOCK with STAT=STAT_LOCKED_OTHER_IMAGE specifier

      program lock_unlock

         USE, INTRINSIC :: ISO_FORTRAN_ENV

         IMPLICIT NONE
         type(LOCK_TYPE) :: lock_var [*]
         type(LOCK_TYPE), allocatable  :: a_lock_var [:]
         integer :: me, stat_var, i
         logical :: error

        me = this_image()

        if(NPROCS ==  1) then
          print * , "Config Error: NPROCS should be greater than 1"
        end if

        error = .false.
        stat_var = 0

        ! image 1 and image 2 acquire local locks

        if (me == 1 .or. me == 2) then
            LOCK ( lock_var )
        end if

        sync all

        ! image 1 and image 2 attempt to unlock their locks (should be fine)

        if (me == 1 .or. me == 2) then
            UNLOCK ( lock_var, stat=stat_var )
            if (stat_var /= 0) then
                print *, "Error: unsuccessfuly releasing of held lock - ", &
                         "stat_var = ", stat_var
                error = .true.
            end if

            ! grab lock again
            LOCK ( lock_var )
        end if

        sync all

        ! image 1 and image 2 try to unlock the other's lock

        if (me == 1) then
            UNLOCK ( lock_var[2], stat=stat_var )
        else if (me == 2) then
            UNLOCK ( lock_var[1], stat=stat_var )
        end if

        if (me == 1 .or. me == 2) then
            if (stat_var /= STAT_LOCKED_OTHER_IMAGE) then
                print *, "Error: stat_var = ", stat_var, &
                         ", should have been ", STAT_LOCKED_OTHER_IMAGE
                error = .true.
            end if

        end if

        sync all

        !! now try with allocatable lock

        ALLOCATE (a_lock_var [*])

        stat_var = 0

        ! image 1 and image 2 acquire local locks

        if (me == 1 .or. me == 2) then
            LOCK ( a_lock_var )
        end if

        sync all

        ! image 1 and image 2 attempt to unlock their locks (should be fine)

        if (me == 1 .or. me == 2) then
            UNLOCK ( a_lock_var, stat=stat_var )
            if (stat_var /= 0) then
                print *, "Error: unsuccessfuly releasing of held lock - ", &
                         "stat_var = ", stat_var
                error = .true.
            end if

            ! grab lock again
            LOCK ( a_lock_var )
        end if

        sync all

        ! image 1 and image 2 try to unlock the other's lock

        if (me == 1) then
            UNLOCK ( a_lock_var[2], stat=stat_var )
        else if (me == 2) then
            UNLOCK ( a_lock_var[1], stat=stat_var )
        end if

        if (me == 1 .or. me == 2) then
            if (stat_var /= STAT_LOCKED_OTHER_IMAGE) then
                print *, "Error: stat_var = ", stat_var, &
                         ", should have been ", STAT_LOCKED_OTHER_IMAGE
                error = .true.
            end if

        end if

        DEALLOCATE (a_lock_var)

        if (error) then
            call EXIT(1)
        end if

    end program

