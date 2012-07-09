      module sync_images_tests
        contains

        integer function test_sync_images()
          integer, save :: num[*]
          integer :: pass = 1
          integer :: ret

          num = 0
          sync all

          if (this_image() == 1) then
            call sleep(SLEEP)
            num = 1
            sync images(*)
          else
            sync images(1)
            if (num[1] == 1) then
              pass = 1
            else
              pass = 0
            end if
          end if

          sync all

          test_sync_images = pass

        end function

        integer function crosstest_sync_images()
          integer, save :: num[*]
          integer :: pass = 1
          integer :: ret

          num = 0
          sync all

          if (this_image() == 1) then
            call sleep(SLEEP)
            num = 1
          else
            if (num[1] == 1) then
              pass = 1
            else
              pass = 0
            end if
          end if

          sync all
          crosstest_sync_images = pass
        end function
      end module

      program sync_images_test

        use :: sync_images_tests

        implicit none
        integer :: i
        integer, parameter :: N=5
        integer :: r
        integer :: p1[*], p2[*]
        integer, allocatable :: q(:)

        p1 = 0
        p2 = 0

        allocate( q(num_images()) )

        do i=1,N
          p1 = p1 + test_sync_images()
        end do

        do i=1,N
          p2 = p2 + crosstest_sync_images()
        end do

        sync all
        if (this_image() == 1) then
          do i=1,num_images()
            q(i) = p1[i]
          end do
          p1 = minval(q(:))
          print *, "q = ", q

          do i=1,num_images()
            q(i) = p2[i]
          end do
          p2 = minval(q(:))
          print *, "q = ", q

          print *, " test: ", (p1*100.0) / N, ", crosstest: ",          &
                    100 - (p2*100.0) / N, "%"
        end if

      end program
