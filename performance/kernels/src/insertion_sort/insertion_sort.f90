      ! SORT INTEGER USING MULTIPLE IMAGES
      !
      ! This program uses coarray of integer and sorts the array.
      ! Each image does an insertion sort to sort the task distributed
      ! to it. (Other sorts can be used for improvement).
      ! When each image has sorted chunks, they are merged in O(log n) time

      program customerdb
        implicit none

        interface
          subroutine sort(customerList,t,startIndex,endIndex)
            integer, intent(in) :: startIndex, endIndex, t
            integer, dimension(t), intent(inout) :: customerList[*]
          end subroutine sort
        end interface

        integer, parameter :: totalCount = 100000
        integer :: customers(totalCount)[*]
        integer, allocatable :: work1(:), work2(:)
        integer :: width, width1, width2, cw1, cw2, r
        integer :: startIndex[*], endIndex[*], palSIndex, palEIndex
        integer :: n=0, st, i, j, k, me, mypal, dim, rand_seed
        integer :: mylog2, find_mypal
        integer :: time_array(8)
        real :: diff, rand_num
        integer :: ticks, start_time, end_time, rate


        me = this_image()
        if (me == 1) then
            ! create random set of data for test sort
            do i=1,totalCount
                rand_seed = i
                call random_seed(rand_seed)
                call random_number(rand_num)
                customers(i) = int(rand_num*totalCount+1)
            end do
            ! distribute data set to other images
            do i=2, num_images()
                customers(:)[i] = customers
            end do
        end if

        sync all

        ! start timer after task is being divided
        call system_clock(start_time, rate)

        ! divide task
        call distribute_chunks(me, totalCount, startIndex, endIndex)
        !write (*, 1001) (customers(i), i=startIndex,endIndex)

        ! sort chunk
        call sort(customers,totalCount,startIndex,endIndex)

        ! merge results
        dim = mylog2(num_images())
        if (dim .ne. 0) then
            do i=1, dim
                mypal = find_mypal(me,i)
                if (mypal .gt. num_images()) then
                    cycle
                end if

                !print *, this_image(), " ", mypal

                sync images(mypal)

                if (this_image() < mypal) then

                palSIndex = startIndex[mypal]
                palEIndex = endIndex[mypal]
                width1 = palEIndex - palSIndex + 1 ! plus 1 for inclusive
                allocate(work1(width1))
                work1 (1:width1) = customers(palSIndex:palEIndex)[mypal]

                width2 = endIndex - startIndex + 1 ! plus 1 for inclusive
                allocate(work2(width2))
                work2 (1:width2) = customers(startIndex:endIndex)

                ! merge between work1 and work2
                startIndex = min(startIndex, palSIndex)
                endIndex = max(endIndex, palEIndex)
                width = endIndex - startIndex + 1 ! plus 1 for inclusive
                cw1 = 1
                cw2 = 1
                r = startIndex
                do j = 1, width
                    if (cw1 .gt. width1) then
                        do k=cw2, width2
                            customers(r) = work2(cw2)
                            r = r + 1
                            cw2 = cw2 + 1
                        end do
                        exit
                    end if

                    if (cw2 .gt. width2) then
                        do k=cw1, width1
                            customers(r) = work1(cw1)
                            r = r + 1
                            cw1 = cw1 + 1
                        end do
                        exit
                    end if

                    if (work1(cw1) < work2(cw2)) then
                        customers(r) = work1(cw1)
                        cw1 = cw1 + 1
                    else !if (work2(cw2) < work1(cw1)) then
                        customers(r) = work2(cw2)
                        cw2 = cw2 + 1
                    end if

                    r = r + 1

                end do
                !if (me == 1) then
                    !print *, 'width1, width2, width : ', width1, width2, width
                    !write (*, 1001)  (work1(k), k=1, width1)
                    !print *, '--------------'
                    !write (*, 1001) (work2(k), k=1, width2)
                    !print *, '--------------'
                !end if
                deallocate(work1)
                deallocate(work2)

                end if

                sync images (mypal)
            end do
        end if

        sync all
        call system_clock(end_time)


        if (me == 1) then
            print *, "Sorting Integer on array size ", totalCount
        ticks = end_time - start_time
        write(*, '(//A20,I8,A)')   "clock rate = ", rate, " ticks/s"
        write(*, '(A20,I8)')           "ticks  = ", ticks
        write(*, '(A20,F8.2,A)') "elapsed time = ", ticks/(1.0*rate), &
	& " seconds"
        end if

      end program customerdb

      subroutine distribute_chunks(me, total, sIndex, eIndex)
        implicit none
        integer, intent(in) :: me, total
        integer, intent(out) :: sIndex, eIndex
        integer :: width
        integer :: m,l1,l2,l

        m = mod(total, num_images())
        l1 = total / num_images() + 1
        l2 = total / num_images()

        if (me <= m) then
            l = l1
        else
            l = l2
        end if

        if (me <= m) then
            sIndex = l1*(me-1) + 1
            eIndex = sIndex + l1 -1
        else
            sIndex = m*l1 + (me-m-1)*l2 + 1
            eIndex = sIndex + l2 - 1
        end if

      end subroutine

      subroutine sort(customerList,t,startIndex,endIndex)
        implicit none
        integer, intent(in) :: startIndex, endIndex, t
        integer, dimension(t), intent(inout) :: customerList[*]
        integer :: temp
        integer :: i,j,m

        do i=startIndex, endIndex-1
            m = i
            do j=i+1,endIndex
                if (customerList(j) .lt. customerList(m)) then
                    m = j
                end if
            end do

            if (m /= i) then
                temp = customerList(i)
                customerList(i) = customerList(m)
                customerList(m) = temp
            end if
        end do
      end subroutine sort

      function mylog2(n) result(l)
        integer :: l
        integer, intent(in) :: n
        l = ceiling(log(real(n))/log(2.))
      end function mylog2

      function find_mypal(img, iter) result(l)
        integer :: l, denominator, hop
        integer, intent(in) :: img, iter

        denominator = 2**iter
        hop = 2**(iter-1)
        offset = mod(img, denominator)

        ! wrap around the offset so that lowest image number
        ! will have offset 0 and highest image number will have
        ! offset equal to denominator. Then less 1 to make it start
        ! from zero
        ! e.g. iter 1 can cause offsets 1,2
        !      iter 2 can cause offsets 0,1,2,3
        if (offset .eq. 0) then
            offset = denominator
        end if
        offset = offset -1

        if (offset < denominator/2) then
            l = img + hop
        else
            l = img - hop
        end if
        !print *,iter,img,l,'hop: ',hop,'deno: ',denominator,'off: ',offset
      end function find_mypal

