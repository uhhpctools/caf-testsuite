!compilation should fail, but somehow its running
!if coarray object is an array, we should employ the parantheses whenever accessing the coarray object
program main
	implicit none
	integer :: a(5)[*]
	integer :: rank
	
	rank = this_image()
	a[rank] = rank
	
	write(*,*) "Image-",rank,"=",a(2)[rank]
	
end program main
