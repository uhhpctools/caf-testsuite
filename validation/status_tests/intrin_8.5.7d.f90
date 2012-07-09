! Testing for STAT_LOCKED status

      program stat_locked_status

        USE, INTRINSIC :: ISO_FORTRAN_ENV

        IMPLICIT NONE
        type(LOCK_TYPE) :: lock_var [*]
        type(LOCK_TYPE), allocatable  :: a_lock_var [:]
        integer :: me, stat_var, err_cnt

        ALLOCATE(a_lock_var [*])

        me = this_image()
        err_cnt = 0

        if(NPROCS ==  1) then
            print * , "Config Error: NPROCS should be greater than 1"
        end if

        stat_var = -1

        if (me == 1) then
            LOCK(lock_var, STAT=stat_var)
            if (stat_var /= 0) then
                print *, "Error: first lock attempt, stat_var = ", stat_var
                err_cnt = 1
            end if
            LOCK(lock_var, STAT=stat_var)
            if (stat_var /= STAT_LOCKED) then
                print *, "Error: second lock attempt, stat_var = ", stat_var
                err_cnt = 1
            end if
            UNLOCK(lock_var)
        end if

        ! ALLOCATABLE LOCK_TYPE
        stat_var = -1

        if (me == 1) then
            LOCK(a_lock_var, STAT=stat_var)
            if (stat_var /= 0) then
                print *, "Error: first lock attempt, stat_var = ", stat_var
                err_cnt = 1
            end if
            LOCK(a_lock_var, STAT=stat_var)
            if (stat_var /= STAT_LOCKED) then
                print *, "Error: second lock attempt, stat_var = ", stat_var
                err_cnt = 1
            end if
            UNLOCK(a_lock_var)
        end if

        if (err_cnt == 1) then
            call EXIT(1)
        end if

      end program

