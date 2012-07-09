! Testing for LOCK with ACQUIRED_LOCK= specifier. 
! Case: Successful locking

      program lock_unlock

        USE, INTRINSIC :: ISO_FORTRAN_ENV

        IMPLICIT NONE
        type(LOCK_TYPE) :: lock_var [*]
        logical :: acq_stat
        integer :: me, stat_var, err_cnt


        me = this_image()

        if(NPROCS ==  1) then
          print * , "Config Error: NPROCS should be greater than 1"
        end if

	acq_stat=.FALSE.

        if (me == 1) then
          LOCK(lock_var, ACQUIRED_LOCK=acq_stat)
          if (acq_stat) then
	    print *, "acq_stat == .TRUE.: ", me
	  else
            print *, "Error:acq_stat /= .TRUE.: ", me
            err_cnt = 1
          end if
          UNLOCK(lock_var,STAT=stat_var)
        end if

        if (err_cnt == 1) then
          call EXIT(1)
        end if

      end program

