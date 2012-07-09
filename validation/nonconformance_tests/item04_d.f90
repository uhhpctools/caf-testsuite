!access to coarray should be with valid index
program main
	implicit none
	integer :: a[*], rank, i, total

	rank = this_image()

	if (rank == 1) then
		total = num_images()+1
		a[total] = total
	end if

end program main
