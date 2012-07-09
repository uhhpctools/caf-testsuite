!Allocatable arrrays
!allocatable coarray without save attribute in a procedure will be deallocated automatically when exit from procedure
!????
program main
implicit none
	integer, allocatable :: x(:)[:,:]
	integer :: temp(10)
	
	call subr(10,x)
	
!	if(this_image() .eq. 1) then
		temp = x(:)[1,1]
		write(*,*)"x:", temp
!		if(x(1) .ne. 100) then
!			write(*,*)"Error runtime: item9_d"
!		end if
!	end if

contains
	subroutine subr(n,x)
		integer :: n
		integer, allocatable :: x(:)[:,:]

		allocate(x(n)[2,*])
		x(1:n)[1,1] = 100
		!write(*,*)"x:", x(:)[1,1]
	end subroutine subr
end program main
