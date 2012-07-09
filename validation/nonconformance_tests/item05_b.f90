!testing allocatable coarray argument with intent(out)

program main
implicit none
	real, allocatable :: a(:)[:]
	call subr(a)
	!sync all()
	call caf_sync_all()
	write(*,*) "Rank -",this_image()," =>",a(1)
contains 
subroutine subr(x)
	real, allocatable, intent(out) :: x(:)[:]
	
	allocate(x(2)[*])
	x(1)[this_image()] = this_image()
	
end subroutine subr	
end program main
