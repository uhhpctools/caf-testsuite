! For a coindexed object, its cosubscript list determines the image
! index in the same way that a subscript list determines the subscript
! order value for an array element

       program cosubscript_test
       implicit none

       integer, parameter :: X = 3, Y = 2
       integer, parameter :: P = 1, Q = -1
       integer :: me
       integer :: i,j,k

       integer :: scalar[0:P, -1:Q, *]
       integer :: array(X,Y)[0:P, -1:Q, *]
       integer, allocatable :: alloc_scalar[:,:,:]
       integer, allocatable :: alloc_array(:,:)[:,:,:]

       integer :: dim3_max, counter
       logical :: is_err


       allocate (alloc_scalar[0:P,-1:Q,*])
       allocate (alloc_array(X,Y)[0:P,-1:Q,*])

       is_err = .false.

       me = this_image()

       array    = me
       scalar   = me

       alloc_array    = me
       alloc_scalar   = me

       dim3_max = NPROCS / ( (P+1)*(Q+2) )

       if (MOD(NPROCS,((P+1)*(Q+2))) .ge. 1) then
         dim3_max = dim3_max+1
       end if

       sync all

       ! ******* SCALAR ***********
       counter = 0
       do k =1, dim3_max
         do j = -1,Q
           do i = 0,P
            counter = counter+1
            if (counter /= scalar[i,j,k]) then
               print * , "Error in cosubscript translation "
               print * , "[", i,",",j,",",k,"]/=",counter
               is_err = .true.
            end if
           end do
         end do
      end do


      ! *********** ARRAY ****************
      ! testing for allocatable coindexed int_arrayay
      counter = 0
      do k =1, dim3_max
         do j = -1,Q
           do i = 0,P
            counter = counter+1
            if (counter /= array(1,1)[i,j,k]) then
               print * , "Error is cosubscript translation "
               print * , "[", i,",",j,",",k,"]/=",counter
               is_err = .true.
            end if
           end do
         end do
      end do

       ! ******* ALLOCATABLE SCALAR ***********
       counter = 0
       do k =1, dim3_max
         do j = -1,Q
           do i = 0,P
            counter = counter+1
            if (counter /= alloc_scalar[i,j,k]) then
               print * , "Error in cosubscript translation "
               print * , "[", i,",",j,",",k,"]/=",counter
               is_err = .true.
            end if
           end do
         end do
      end do


      ! *********** ALLOCATABLE COARRAY ****************
      ! testing for allocatable coarray
      counter = 0
      do k =1, dim3_max
         do j = -1,Q
           do i = 0,P
            counter = counter+1
            if (counter /= alloc_array(1,1)[i,j,k]) then
               print * , "Error is cosubscript translation "
               print * , "[", i,",",j,",",k,"]/=",counter
               is_err = .true.
            end if
           end do
         end do
      end do


      if (is_err) then
           STOP 1
      end if

      end program cosubscript_test
