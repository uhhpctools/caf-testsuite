!automatic coarrays are not permitted
!If the automatic coarray is an array, compilation error in openuh: ""X" is an automatic variable.  It must not have the co-array DIMENSION attribute."
!If the automatic coarray is scalar, compilation is successful but when it's called, the value of the coarray object is local to the image
program main
    implicit none
	integer :: x[*]

	x = this_image()
	write(*,*) "x:",x
	call subr(2)

contains
subroutine subr(n)
	integer :: n
	real :: x(n)[*]   ! page 9 of N1824... should compile

	x(:)[this_image()] = this_image()
end subroutine subr
end program main
