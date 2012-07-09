!a subroutine with non-allocatable non-dummy coarray should have the SAVE attribute

program main
    implicit none
    integer :: x[*]

    x = this_image()

    if(this_image() .eq. 1) then  !only image 1 call subr
      call subr(2)
    end if

    sync all

    contains

    subroutine subr(num)
      integer :: num
      integer :: abc[*]
      abc[num] = 12
    end subroutine subr

end program main
