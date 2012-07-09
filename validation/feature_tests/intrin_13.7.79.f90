!verification of image_index()

program main

implicit none
    integer :: x[2,*]
    integer :: idx, size, temp
    integer :: sub(2)
    integer :: i, j
    temp = 1
    size = NPROCS

    if (NPROCS .le. 3 ) then
      print * , "ERROR: NPROCS should be greater than 3"
      call EXIT(1)
    end if

    do j = 1 ,(size/2)
      do i = 1,2
        sub(1) = i
        sub(2) = j
        idx = image_index(x,sub)
        if(idx .ne. temp) then
          write(0,*) "ERROR: images index returned ", idx ,"instead of ", temp
          call EXIT(1)
        end if
        temp = temp+1
      end do
    end do

end program main
