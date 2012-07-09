      module matrix_multiply

          private

          public :: first_alg, last_alg
          public :: alg1, alg2, alg3
          public :: init_data, do_matrix_multiply, free_data

          ! algorithm selection
          integer, parameter :: alg1 = 1
          integer, parameter :: alg2 = 2
          integer, parameter :: alg3 = 3
          integer, parameter :: alg4 = 4

          integer, parameter :: first_alg = alg1
          integer, parameter :: last_alg  = alg4

          ! data
          double precision, allocatable :: a(:,:)[:,:]
          double precision, allocatable :: b(:,:)[:,:]
          double precision, allocatable :: c(:,:)[:,:]

          logical :: do_check
          double precision, allocatable :: a_check(:,:)
          double precision, allocatable :: b_check(:,:)
          double precision, allocatable :: c_check(:,:)


          contains

          subroutine init_data(n,p)
            implicit none
            integer :: n,p

            integer :: rand_seed
            real    :: real_rand_num
            integer :: myP, myQ, i, j

            do_check = .false.
            allocate(a(n/p,n/p)[p,*], b(n/p,n/p)[p,*], c(n/p,n/p)[p,*])

            if (this_image() == 1 .and. n <= 1000) then
                do_check = .true.
            end if

            myP = this_image(a,1)
            myQ = this_image(a,2)

            ! initialize matrices
            do i=1,n/p
              do j=1,n/p
                rand_seed = i*j
                call random_seed(rand_seed)
                call random_number(real_rand_num)
                a(j,i) = real_rand_num*n/p
                call random_number(real_rand_num)
                b(j,i) = real_rand_num*n/p
              end do
            end do

            sync all

            if (do_check) then
                allocate(a_check(n,n), b_check(n,n), c_check(n,n))
                do i=1,p
                  do j=1,p
                    a_check((j-1)*n/p+1:j*n/p, (i-1)*n/p+1:i*n/p) &
                              = a(:,:)[j,i]
                    b_check((j-1)*n/p+1:j*n/p, (i-1)*n/p+1:i*n/p) &
                              = b(:,:)[j,i]
                  end do
                end do
               c_check(:,:) = matmul(a_check(:,:), b_check(:,:))
            end if

          end subroutine init_data

          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          !
          !
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          subroutine matmul1()
              implicit none
              integer :: i,j,l
              integer :: m, p
              integer :: my_p1, my_p2

              m = size(c,1)
              p = ucobound(c,1) - lcobound(c,1) + 1
              my_p1 = this_image(a,1)
              my_p2 = this_image(b,2)

              sync all

              do i = 1, m
                do j = 1, m
                  do l = 1, p
                    c(i,j) = c(i,j) + &
                      sum(a(i,:)[my_p1,l] * b(:,j)[l,my_p2])
                  end do
                end do
              end do

              sync all
          end subroutine matmul1

          subroutine matmul2()
              implicit none
              integer :: k
              integer :: m1, m2, p
              integer :: my_p1, my_p2

              double precision, allocatable :: res(:,:)

              m1 = size(c,1)
              m2 = size(c,2)
              p = ucobound(c,1) - lcobound(c,1) + 1
              my_p1 = this_image(a,1)
              my_p2 = this_image(b,2)

              allocate( res(m1,m2) )

              sync all

              do k = 1, p
                res = matmul(a(:,:)[my_p1,k], b(:,:)[k,my_p2])
                c(:,:) = c(:,:) + res(:,:)
              end do

              sync all

              deallocate( res )

          end subroutine matmul2

        subroutine broadcast_ring_in_row(a, nr, root)
              implicit none
              integer :: nr, root
              double precision :: a(:,:)[nr, *]

              integer :: log2_p, p, q
              integer :: my_row, my_col, step, partner
              integer :: e, w, e_index, w_index

              my_row = this_image(c, 1)
              my_col = this_image(c, 2)
              p = ucobound(a, 2)

              ! east and west neighbors
              if (my_col == 1) then
                w = p
              else
                w = my_col - 1
              end if
              w_index = image_index(a, [my_row, w])
              if (my_col == p) then
                e = 1
              else
                e = my_col + 1
              end if
              e_index = image_index(a, [my_row, e])

              if (my_col == root) then
                a(:,:)[my_row, e] = a(:,:)
                sync images ( e_index )
              else
                sync images ( w_index )
                if (e /= root) then
                  a(:,:)[my_row, e] = a(:,:)
                  sync images ( e_index )
                end if
              end if
        end subroutine broadcast_ring_in_row

        subroutine broadcast_ring_in_column(a, nr, root)
              implicit none
              integer :: nr, root
              double precision :: a(:,:)[nr, *]

              integer :: log2_p, p, q
              integer :: my_row, my_col, step, partner
              integer :: n, s, n_index, s_index

              my_row = this_image(c, 1)
              my_col = this_image(c, 2)
              p = nr

              ! get north and south neighbors
              if (my_row == 1) then
                n = p
              else
                n = my_row - 1
              end if
              n_index = image_index(a, [n, my_col])
              if (my_row == nr) then
                s = 1
              else
                s = my_row + 1
              end if
              s_index = image_index(a, [s, my_col])

              if (my_row == root) then
                a(:,:)[s,my_col] = a(:,:)
                sync images ( s_index )
              else
                sync images ( n_index )
                if (s /= root) then
                  a(:,:)[s,my_col] = a(:,:)
                  sync images ( s_index )
                end if
              end if
        end subroutine broadcast_ring_in_column


        subroutine broadcast_in_row(a, nr, root)
              implicit none
              integer :: nr, root
              double precision :: a(:,:)[nr,*]

              integer :: log2_p, p, q
              integer :: my_row, my_col, step, partner

              my_row = this_image(a, 1)
              my_col = this_image(a, 2)
              p = ucobound(a, 2)

              ! find greatest power of 2
              q = 1
              log2_p = 0
              do while(2*q <= p)
                q = 2*q
                log2_p = log2_p + 1
              end do
              if (q < p) then
                  log2_p = log2_p + 1
              end if


              ! copy from root column to column 1
              if (my_col == root) then
                if (my_col /= 1) then
                  a(:,:)[my_row, 1] = a(:,:)
                  sync images( image_index(a, [my_row,1]) )
                end if
              else
                if (my_col == 1) then
                  sync images( image_index(a, [my_row,root]) )
                end if
              end if

              ! broadcast from column 1

              step = q
              do while (step > 0)
                if ( mod(my_col-1,step) == 0 ) then
                  if ( mod(my_col-1,2*step) == 0 ) then
                    if ( (my_col+step) <= p ) then
                        partner = my_col + step
                        a(:,:)[my_row, partner] = a(:,:)
                        sync images(image_index(a, [my_row,partner]))
                    end if
                  else
                    if ( (my_col-step) >= 1 ) then
                        partner = my_col - step
                        sync images(image_index(a, [my_row,partner]))
                    end if
                  end if
                end if
                step = step / 2
              end do

          end subroutine broadcast_in_row

        subroutine broadcast_in_column(a, nr, root)
              implicit none
              integer :: nr, root
              double precision :: a(:,:)[nr,*]

              integer :: log2_p, p, q
              integer :: my_row, my_col, step, partner

              my_row = this_image(a, 1)
              my_col = this_image(a, 2)
              p = nr

              ! find greatest power of 2
              q = 1
              log2_p = 0
              do while(2*q <= p)
                q = 2*q
                log2_p = log2_p + 1
              end do
              if (q < p) then
                  log2_p = log2_p + 1
              end if


              ! copy from root row to row 1
              if (my_row == root) then
                if (my_row /= 1) then
                  a(:,:)[1, my_col] = a(:,:)
                  sync images( image_index(a, [1,my_col]) )
                end if
              else
                if (my_row == 1) then
                  sync images( image_index(a, [root,my_col]) )
                end if
              end if

              ! broadcast from row 1

              step = q
              do while (step > 0)
                if ( mod(my_row-1,step) == 0 ) then
                  if ( mod(my_row-1,2*step) == 0 ) then
                    if ( (my_row+step) <= p ) then
                        partner = my_row + step
                        a(:,:)[partner, my_col] = a(:,:)
                        sync images(image_index(a, [partner,my_col]))
                    end if
                  else
                    if ( (my_row-step) >= 1 ) then
                        partner = my_row - step
                        sync images(image_index(a, [partner,my_col]))
                    end if
                  end if
                end if
                step = step / 2
              end do

          end subroutine broadcast_in_column

          subroutine summa()
              implicit none
              integer :: k
              integer :: m1, m2, p
              integer :: my_p1, my_p2

              double precision, allocatable :: res(:,:)
              double precision, allocatable :: work1(:,:)[:,:]
              double precision, allocatable :: work2(:,:)[:,:]

              m1 = size(c,1)
              m2 = size(c,2)
              p = ucobound(c,1) - lcobound(c,1) + 1
              my_p1 = this_image(a,1)
              my_p2 = this_image(b,2)

              allocate( res(m1,m2) )
              allocate( work1(size(a,1),size(a,2))[p,*] )
              allocate( work2(size(b,1),size(b,2))[p,*] )

              do k = 1, p
                ! broadcast: work1(:,:)[my_p1, :] <- a(:,:)[my_p1, k]
                ! broadcast: work2(:,:)[:, my_p2] <- b(:,:)[k, my_p2]
                if (my_p2 == k) work1(:,:) = a(:,:)
                if (my_p1 == k) work2(:,:) = b(:,:)

                sync all
                call broadcast_ring_in_row(work1, p, k)
                call broadcast_ring_in_column(work2, p, k)
                sync all

                res = matmul(work1(:,:), work2(:,:))
                c(:,:) = c(:,:) + res(:,:)
              end do

              deallocate( res, work1, work2 )
          end subroutine summa

          subroutine cannon()
              implicit none
              integer :: k,j
              integer :: m1, m2, p, q
              integer :: my_p1, my_p2

              integer :: n, s, e, w
              integer :: neighbors(4)

              double precision, allocatable :: res(:,:)
              double precision, allocatable :: work1(:,:)[:]
              double precision, allocatable :: work2(:,:)[:]

              m1 = size(c,1)
              m2 = size(c,2)
              p = ucobound(c,1) - lcobound(c,1) + 1
              my_p1 = this_image(a,1)
              my_p2 = this_image(b,2)

              allocate( res(m1,m2) )
              allocate( work1(size(a,1),size(a,2))[*] )
              allocate( work2(size(b,1),size(b,2))[*] )

              ! redistribute work1
              do k = 1, p
                work1(:,:) = a(:,:)[my_p1, mod(my_p2+k-2,p)+1]
                work2(:,:) = b(:,:)
              end do

              ! get north, south, east, and west neighbors
              if (my_p1 == 1) then
                n = image_index(c, [p, my_p2])
              else
                n = image_index(c, [my_p1-1, my_p2])
              end if
              if (my_p1 == p) then
                s = image_index(c, [1, my_p2])
              else
                s = image_index(c, [my_p1+1, my_p2])
              end if
              if (my_p2 == 1) then
                e = image_index(c, [my_p1, p])
              else
                e = image_index(c, [my_p1, my_p2-1])
              end if
              if (my_p2 == p) then
                w = image_index(c, [my_p1, 1])
              else
                w = image_index(c, [my_p1, my_p2+1])
              end if

              do k = 1, p
                res = matmul(work1(:,:), work2(:,:))
                c(:,:) = c(:,:) + res(:,:)

                j = 1
                neighbors(j) = n
                if (s /= n) then
                  j = j + 1
                  neighbors(j) = s
                end if
                if (w /= s) then
                  j = j + 1
                  neighbors(j) = w
                end if
                if (e /= w) then
                  j = j + 1
                  neighbors(j) = e
                end if

                sync images( neighbors(1:j) )
                work1(:,:) = work1(:,:)[n]
                work2(:,:) = work2(:,:)[e]
              end do

              deallocate( res, work1, work2 )
          end subroutine cannon

          subroutine do_matrix_multiply(alg)
              implicit none
              integer :: alg
              character (len=20) :: alg_name
              integer :: ticks, start_time, end_time, rate
              integer :: m, p
              integer :: i, j
              double precision :: sum1, sum2

              m = size(c,1)
              p = ucobound(c,1) - lcobound(c,1) + 1

              c(:,:) = 0.0

              select case(alg)
                case(alg1)
                    alg_name = "naive"
                    call system_clock(start_time, rate)
                    call matmul1()
                    call system_clock(end_time)
                case(alg2)
                    alg_name = "default"
                    call system_clock(start_time, rate)
                    call matmul2()
                    call system_clock(end_time)
                case(alg3)
                    alg_name = "SUMMA"
                    call system_clock(start_time, rate)
                    call summa()
                    call system_clock(end_time)
                case(alg4)
                    alg_name = "Cannon's"
                    call system_clock(start_time, rate)
                    call cannon()
                    call system_clock(end_time)
                case default
                    alg_name = "default"
                    call system_clock(start_time, rate)
                    call matmul1()
                    call system_clock(end_time)
              end select

              if (this_image() == 1) then
                  print *, alg_name
                  ticks = end_time - start_time
                  write(*, '(A20,I8,A)')   "clock rate = ", &
                                           rate, " ticks/s"
                  write(*, '(A20,I8)')     "ticks  = ", ticks
                  write(*, '(A20,F8.2,A)') "elapsed time = ", &
                                           ticks/(1.0*rate), " seconds"

                  if (do_check) then
                     do i=1,p
                       do j=1,p
                          c(:,:) = c(:,:)[j,i]
                          sum1 = sum(c(:,:))
                          sum2 = sum( &
                           c_check((j-1)*m+1:j*m,(i-1)*m+1:i*m))
                          write (*,'(i4, i4, ": error sum % = ", f8.5)') &
                              j, i, (sum1-sum2)/sum1
                       end do
                     end do

                  end if

              end if

          end subroutine do_matrix_multiply

          subroutine free_data()
              deallocate(a, b, c)
              if (do_check) then
                  deallocate(a_check, b_check, c_check)
              end if
          end subroutine free_data
      end module matrix_multiply


      program mm
          use matrix_multiply
          implicit none

          integer :: n[*],p
          integer :: ti, ni
          integer :: nargs
          character (len=10) :: arg
          integer :: i

          ti = this_image()
          ni = num_images()

          if (ti == 1) then
            nargs = command_argument_count()
            if (nargs /= 1) then
                n = 1000
            else
                call get_command_argument(1, arg)
                read (arg, '(i120)') n
            end if
            print *, "Using n = ", n
          end if

          sync all

          n = n[1]

        ! check that ni is a square
        p = INT(sqrt(REAL(ni)))
        if ( p*p /= ni ) then
          if (ti == 1)   &
            write(*,"('num_images must be square: p=',i5)") ni
          stop
        end if
        if ( mod(n,p) /= 0) then
          if (ti == 1)   &
            write(*,"('n must be a multiple of ',i5)") p
          stop
        end if

        call init_data(n,p)

        do i = alg2, last_alg

          sync all
          call do_matrix_multiply(i)

        end do

        call free_data()

      end program mm
