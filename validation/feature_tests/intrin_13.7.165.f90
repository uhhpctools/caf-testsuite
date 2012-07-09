!this_image(coarray, [dim])

    program main
      integer :: arr[2,1,*], dim
      integer :: tmp(2,1,2), int_tmp
      integer :: rank, size
      integer :: cosubs(3)

      tmp = reshape((/1, 2, 3, 4/),shape(tmp))

      rank = this_image()
      cosubs=this_image(arr)

  

      if (cosubs(1) == this_image(arr,1) .AND. cosubs(2) == this_image(arr,2) &
          .AND. cosubs(3) == this_image(arr,3) ) then
           if( ((2*1)*(cosubs(3)-1)+ 2*(cosubs(2)-1) + cosubs(1)) /= rank ) then
              print * , "ERROR this_image(coarray)"
              STOP 1
           end if
      else
           print *, "ERROR this_image(coarray,dim)"
           STOP 1
      end if
   end program main
