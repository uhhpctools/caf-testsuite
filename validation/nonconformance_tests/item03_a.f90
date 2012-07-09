!compilation should fail
!can not declare a coarray object as constant
program main
	implicit none
	integer, PARAMETER :: pi[*] = 3
end program main

