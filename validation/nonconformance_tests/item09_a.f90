!Allocatable arrrays
!cobounds must be included in the allocate statement
!openuh caf : error compilation "The number of extents specified for this allocate object does not match the declared rank."
!g95 : error compilation "Error: Coarray specification at (1) must follow array specification"
program main
implicit none
	integer, allocatable :: x(:)[:]
	allocate(x[*])
end program main
