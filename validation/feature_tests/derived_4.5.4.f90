! Ultimate components of a derived type that are of intrinsic type or
! ALLOCATABLE or POINTER attribute + ultimate components that are of
! derived type and have neither the ALLOCATABLE nor the POINTER
! components

      program derived_4_5_1

      implicit none

      integer :: M=2
      integer :: i, j, cnt, rank , flag, k

       type co_child
         integer,allocatable :: i_all_arr(:,:)[:]
         real,allocatable :: r_all_arr(:,:)[:]
       end type co_child

       type child
         integer,allocatable :: i_all_arr(:,:)
         real,allocatable :: r_all_arr(:,:)
       end type child

       type parent
         type(co_child) ::  co_child_obj
       end type parent

       type co_parent
         type(child) :: child_obj
       end type co_parent

       type(co_parent), allocatable :: co_parent_obj[:]
       type(parent) :: parent_obj

       rank = this_image()

       !.... Initializing all  coarrays by the owning images ....


       !.. Coarray component of non-coarray derived type ..
       allocate(parent_obj%co_child_obj%i_all_arr(M,M)[*])
       allocate(parent_obj%co_child_obj%r_all_arr(M,M)[*])

       !.. Non-Coarray component of coarray derived type ..
       allocate(co_parent_obj[*])
       allocate(co_parent_obj%child_obj%i_all_arr(M,M))
       allocate(co_parent_obj%child_obj%r_all_arr(M,M))

       cnt = 1
       do j = 1 , M
         do i = 1, M

         parent_obj%co_child_obj%i_all_arr(i,j) = cnt*rank
         parent_obj%co_child_obj%r_all_arr(i,j) = cnt*rank*2.0
         co_parent_obj%child_obj%i_all_arr(i,j) = cnt*rank*3
         co_parent_obj%child_obj%r_all_arr(i,j) = cnt*rank*4.0

         cnt = cnt + 1

         enddo
       enddo

       sync all

       flag=0
       do k = 1 , NPROCS
        cnt=1
        do j = 1 , M
         do i = 1, M

          if (                                                         &
           parent_obj%co_child_obj%i_all_arr(i,j)[k] /= cnt*k .or.     &
           parent_obj%co_child_obj%r_all_arr(i,j)[k] /= cnt*k*2.0 .or. &
           co_parent_obj[k]%child_obj%i_all_arr(i,j) /= cnt*k*3 .or.   &
           co_parent_obj[k]%child_obj%r_all_arr(i,j) /= cnt*k*4.0      &
          ) then

                 print *, "Error in ", rank, ":", &
                    parent_obj%co_child_obj%i_all_arr(i,j)[k]
                 flag=1
           endif
           cnt = cnt + 1

         enddo
        enddo
       enddo

       if (flag /= 0) then
         call EXIT(1)
       endif

      end program
