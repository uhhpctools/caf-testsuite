      !Non-coarray components
      !pointer association
      !remote association is not allowed

      program main
        implicit none
        type mytype
          integer :: x
          integer, pointer :: ptr
        end type mytype

        integer, pointer :: localptr
        integer :: rank,i
        type (mytype), target :: mt[*]

        !initialize
        rank = this_image()
        mt%x = rank * 10

        localptr => mt[2]%x

        sync all
        if (this_image() == 1) then
            print *, localptr
        end if

        sync all

      end program main
