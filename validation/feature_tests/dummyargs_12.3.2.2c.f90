! A dummy argument of a procedure is permitted to be a coarray. It may be of
! explicit shape, assumed size, assumed shape, or allocatable

program dummy_arg_test

        implicit none
        integer :: i, tmp(10), me
        integer :: a(10)[3,*], b(10,10)[*], c(2,2)[*]
        integer, allocatable :: d(:)[:]

        me = this_image()
        a = me
        b = me * 2
        c = me * 3

        call subr(10,a,b,c(::2,::2),d)

        sync all
        if (me == 1) then
           do i = 1 , NPROCS
             tmp = i
             if (d(-5)[i] /= tmp(1)) then
                  print * , "ERROR Allocatable dummy argument"
                  call EXIT(1)
             end if
           end do
        end if

        sync all

        deallocate(d)

contains
        subroutine subr(n,w,x,y,z)
                integer :: n
                integer :: w(n)[2,*]    !explicit shape,
                integer :: x(5,0:*)[*]  !assumed size,
                integer :: y(:,:)[*]    !assumed shape
                integer, allocatable :: z(:)[:] !allocatable
                integer :: temp
                integer :: me

                me = this_image()

                allocate(z(-10:-1)[*])
                z = me

                if (lbound(x,1)/=1 .OR.     &
                    lbound(x,2)/=0 .OR.     &
                    ubound(x,1)/=5 ) then

                   print * , "ERROR assumed size"
                   call EXIT(1)

                end if

       end subroutine subr

end program dummy_arg_test
