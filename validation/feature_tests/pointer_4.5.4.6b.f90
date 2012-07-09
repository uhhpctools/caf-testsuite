  ! Specification 10. Non - coarray components
  ! association of pointer components of coarrays with local objects is allowed
  ! pointer association
  ! local association is allowed

  program main
      implicit none
      type mytype
          integer, pointer::ptr(:)
      end type mytype

      integer, target::temp(3)
      integer :: rank
      type (mytype)::mt[*]

      rank = this_image()
      temp(1) = 1 * rank
      temp(2) = 2 * rank
      temp(3) = 3 * rank

      ! component of user define type coarray points to local object
      mt%ptr => temp(:)
      sync all

      if (NPROCS .ge. 2) then
          if(mt[2]%ptr(1) /= 2 .OR. mt[2]%ptr(2) /= 4 &
             .OR. mt[2]%ptr(3) /= 6 ) then

              print *, "ERROR"
              call EXIT(1)

        end if
      end if

      sync all
  end program main
