!Specification 3. The properties of coarrays
!compilation should fail
!a coarray is not permitted to be a pointer
program main
	implicit none
	integer, pointer :: a(2)[*]
end program main
