

      program subobjects

        integer :: arr(10)[*]
        integer :: total_size ,rank, i

        arr(:)= this_image()
        rank = this_image()
        total_size = NPROCS

        sync all

        if  (total_size .gt. 1) then
          if (rank == 1) then
            arr(3:8) = arr(5:10)[2]
            do i = 3, 8
              if(arr(i) /= 2) then
               print *, "ERROR"
               call EXIT(1)
              end if
            end do
           end if
        end if

        sync all

      end program
