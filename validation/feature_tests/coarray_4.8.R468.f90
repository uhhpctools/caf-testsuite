! access to coarray without [] is to local object

    program local_coarray_test
        implicit none
        integer :: a[*]
        logical :: is_error

        is_error = .false.

        a[this_image()] = 100*this_image()

        sync all

        if ( a /= 100*this_image() ) then
            is_error = .true.
        end if

        a = 1000*this_image()

        sync all

        if ( a[this_image()] /= 1000*this_image() ) then
            is_error = .true.
        end if

        if (is_error) then
            STOP 1
        end if

    end program local_coarray_test
