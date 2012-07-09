!function result is not permited to be coarray
!error in compilation in openuh = ""C" is a result-name, therefore it must not be declared with the co-array DIMENSION attribute (identifier first appeared at line 7)"
!error in g95 = "Error: Coarray 'c' at (1) cannot be an automatic variable"
program main
implicit none
	write(*,*)"1+2=",sum(1,2)
contains 
	function sum(a,b) result(c)
		integer, intent(in) :: a,b
		integer :: c[*]
		
		c[this_image()] = a+b
	end function sum
end program main
