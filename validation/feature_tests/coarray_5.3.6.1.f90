! Declare codimensions of a coarray using codimension keyword

    program codimension_keyword

    integer, codimension[*] :: i_sca, i_arr(2,3)
    integer, allocatable, codimension[:] :: i_all_sca, i_all_arr(:,:)
    real, codimension[*] :: r_sca, r_arr(2,3)
    real, allocatable, codimension[:] :: r_all_sca, r_all_arr(:,:)
    integer :: rank

    me = this_image()

    i_sca = me
    i_arr = me
    r_sca = me
    r_arr = me

    allocate (i_all_sca[*])
    allocate (i_all_arr(2,3)[*])
    allocate (r_all_sca[*])
    allocate (r_all_arr(2,3)[*])

    i_all_sca = me
    i_all_arr = me
    r_all_sca = me
    r_all_arr = me

    sync all

    do i = 1 , NPROCS
      print *, "image " , me, "in iteration ", i
      if (i_sca[i] /= i .OR. i_arr(2,2)[i] /= i   .OR.  &
          r_sca[i] /= i .OR. r_arr(2,2)[i] /= i   .OR.  &
          i_all_sca[i] /= i .OR. i_all_arr(2,2)[i] /= i .OR. &
          r_all_sca[i] /= i .OR. r_all_arr(2,2)[i] /= i      &
          ) then
          print *, "Error in supporting of coindexed object on image ", &
          me, " when declared with 'codimension' keyword"
          call EXIT(1)
      end if
    end do

    sync all

    deallocate (i_all_sca)
    deallocate (i_all_arr)
    deallocate (r_all_sca)
    deallocate (r_all_arr)

    end program codimension_keyword
