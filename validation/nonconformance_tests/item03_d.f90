!compilation should fail
!can not initialize a coarray with DATA keyword
program main
	implicit none
	integer :: a(2)[*]
	data a(1)[2] /1/
end program main
