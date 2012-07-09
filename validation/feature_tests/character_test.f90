! a test for coarrays of character type

    program char_test
      character (len=2) :: x(5,5)[*]
      character (len=2) :: s
      logical :: is_error

      is_error = .false.
      x(:,:)='xx'

      sync all

      if(this_image() .eq. 1) then
        x(2:4,2:4) = 'ab'
        x(2:4, 2:4)[2] = 'ab'
        do i = 1, 5
          do j = 1, 5
            s = x(i,j)[2]
            if (x(i,j) /= s) then
                is_error = .true.
            end if
          end do
        end do

      end if

      if (is_error) then
          STOP 1
      end if

   end program

