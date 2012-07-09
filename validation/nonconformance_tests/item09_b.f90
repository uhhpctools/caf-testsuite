!Allocatable arrrays
!cobounds must be included in the allocate statement and upper bound for final codimension must be *
!openuh caf : error compilation "n an ALLOCATE statement for a co-array, the upper bound for the final co-dimension must always be '*'."
!g95 : error compilation "Error: Last upper bound of coarray allocation at (1) must be '*'"
program main
implicit none
	integer, allocatable :: x(:)[:,:]
	allocate(x(10)[2,2])
end program main
