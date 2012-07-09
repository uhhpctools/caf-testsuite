!UCOBOUND(COARRAY[, DIM, KIND])

    program main
        implicit none
        integer :: x_sca[2,*]
        integer :: x_arr(2,3)[-1:0,3:*]
        integer :: dim, kind
        integer :: size, rank
        integer :: ans_sca(2), ans_arr(2)
        integer :: correct_sca(2), correct_arr(2)

        if (num_images() .eq. 4) then
          data correct_sca/2 , 2 /
          data correct_arr/0 , 4 /

          size=NPROCS
          rank = this_image()

          ! without DIM and KIND
          ans_sca(:) = ucobound(x_sca)
          ans_arr(:) = ucobound(x_arr)

          if (ans_sca(1) /=  correct_sca(1) .OR. ans_arr(1) /= correct_arr(1) &
          .AND. ans_sca(2) /=  correct_sca(2) .OR. ans_arr(2) /= correct_arr(2)) then
            print *, "ERROR in UCOBOUND(co_obj)"
            call EXIT(1)
          end if

          !with DIM

          if ( ucobound(x_sca, 1) /= correct_sca(1)  .OR. &
               ucobound(x_sca, 2) /= correct_sca(2)  .OR. &
               ucobound(x_arr, 1) /= correct_arr(1)  .OR. &
               ucobound(x_arr, 2) /= correct_arr(2)) then

             print *, "ERROR in UCOBOUND(co_sca, dim)"
             call EXIT(1)
          end if
         else
           print * , "NPROCS should be equal to 4 for correct testing"
           call EXIT(1)
         end if
    end program main
