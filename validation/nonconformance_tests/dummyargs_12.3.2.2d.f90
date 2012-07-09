!if dummy argument is an allocatable coarray, the actual argument must be an allocatable coarray of same rank and corank
!openuh compilation successful
!execution is also successful
program main
implicit none
	real, allocatable :: a(:)
    integer :: i , rank
    rank = this_image()
	call subr(a)

	sync all
    do i = 1,num_images()
      if (a(1)[i] /= rank ) then
          print *, "ERROR"
      end if
    end do

	contains
    subroutine subr(x)
    	real, allocatable :: x(:)[:]
        allocate(x(2)[*])
    	x(:) = this_image()
    end subroutine subr
end program main
