! Testing for STAT_UNLOCKED status

      program stat_unlocked_stat

         USE, INTRINSIC :: ISO_FORTRAN_ENV

         IMPLICIT NONE
         type(LOCK_TYPE) :: lock_var [*]
         type(LOCK_TYPE), allocatable  :: a_lock_var [:]
         integer :: me, stat_var, i
         logical :: error

        ALLOCATE(a_lock_var [*])

        me = this_image()

        if(NPROCS ==  1) then
            print * , "Config Error: NPROCS should be greater than 1"
        end if

        stat_var = -1
        error = .false.

        if (me == 1) then
            LOCK(lock_var)
            UNLOCK(lock_var, stat=stat_var)
            if (stat_var /= 0) then
                print *, "Error: first unlock attempt, stat_var = ", stat_var
                error = .true.
            end if
            UNLOCK(lock_var, stat=stat_var)
            if (stat_var /= STAT_UNLOCKED) then
                print *, "Error: second unlock attempt, stat_var = ", stat_var
                error = .true.
            end if
        end if

        ! ALLOCATABLE LOCK_TYPE
        stat_var = -1

        if (me == 1) then
            LOCK(a_lock_var)
            UNLOCK(a_lock_var, stat=stat_var)
            if (stat_var /= 0) then
                print *, "Error: first unlock attempt, stat_var = ", stat_var
                error = .true.
            end if
            UNLOCK(a_lock_var, stat=stat_var)
            if (stat_var /= STAT_UNLOCKED) then
                print *, "Error: second unlock attempt, stat_var = ", stat_var
                error = .true.
            end if
        end if

        if (error) then
            call EXIT(1)
        end if

      end program
