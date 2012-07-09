! This program verifies whether the DEALLOCATE & ALLOCATE statements
! act as a barriers.


      program deallocate_test

          integer, allocatable :: arr[:]
          integer, save :: temp[*]
          integer, parameter :: M=10

          if (NPROCS .lt. 2) then
            print *, "Config Error: NPROCS should be > 1"
            call EXIT(1)
          end if

          allocate(arr[*])

          do i  = 1 , M
            temp = i
            deallocate(arr)
            if (temp[2] /= i) then
              print * ,"ERROR: barrier NOT implied"
              call EXIT(1)
            end if
            allocate(arr[*])
          end do

      end program

