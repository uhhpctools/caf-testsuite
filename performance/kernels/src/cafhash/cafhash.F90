! Copyright 2014 The University of Edinburgh

! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at

!     http://www.apache.org/licenses/LICENSE-2.0

! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

! Copyright 2014 HPCTools Group, University of Houston


module initialise
  use iso_fortran_env
  implicit none
  integer :: hashlen, collisions,localhashlen
  integer :: numi, inum
  integer (kind=atomic_int_kind), allocatable :: hashtab(:)[:],hashcount(:)[:]
  type(lock_type) :: iLock[*]

contains

 subroutine inithash()

   numi=num_images()
   inum=this_image()
   hashlen=2*NHASH*numi+1
   localhashlen=2*NHASH + 1
   collisions=0
   allocate(hashtab(localhashlen)[*],hashcount(localhashlen)[*])
   sync all

 end subroutine inithash

 subroutine finhash()
   deallocate(hashtab,hashcount)
 end subroutine finhash

#ifdef USE_AMO
 subroutine hashlookup_with_amo(o,v)
   use iso_fortran_env
   integer*8 :: v
   integer*8 :: lv
   integer :: desti, destpos,localCount
   integer (kind=atomic_int_kind) :: localhash, o, new

   lv=v

   do while (1.gt.0)
      ! determine whom the hash belongs too
      desti = v /localhashlen
      if(desti*localhashlen.lt.v) then
         desti=desti+1
      end if

      destpos = v - (desti - 1)*localhashlen

      new = 0
      call atomic_cas(hashtab(destpos)[desti],localhash,new,o)

      ! get the hash
      if(localhash.eq.0 .or. localhash.eq.o) then
          ! update successful, so increment count and return
          call atomic_add(hashcount(destpos)[desti],1)
         return
      else
          ! its a collision
          collisions=collisions+1
          v=v+1
          if(v.gt.hashlen) then
              v=1
          end if
      end if

   end do
 end subroutine hashlookup_with_amo
#endif

 subroutine hashlookup(o,v)
   integer*8 :: o,v
   integer*8 :: lv
   integer :: desti, destpos,localCount,localHash

   lv=v

   do while (1.gt.0)
      ! determine whom the hash belongs too
      desti = v /localhashlen
      if(desti*localhashlen.lt.v) then
         desti=desti+1
      end if

      destpos = v - (desti - 1)*localhashlen
      ! lock the data
      lock(iLock[desti])
      localhash=hashtab(destpos)[desti]
      ! get the hash
      if(localhash.eq.0) then
        ! insert the entry
         hashtab(destpos)[desti]=o
         hashcount(destpos)[desti]=1
         ! unlock before return
         unlock(ilock[desti])
         return
      else
         ! check to see if it is a collision
         if(localhash.eq.o) then
            ! its a repetition
            hashcount(destpos)[desti]=hashcount(destpos)[desti]+1
            unlock(iLock[desti])
            return
         else
            ! its a collision
            collisions=collisions+1
            v=v+1
            if(v.gt.hashlen) then
               v=1
            end if
            ! unlock before going round the loop
            unlock(iLock[desti])
        end if
      end if


   end do
 end subroutine hashlookup

end module initialise

module co_helper
    contains

    subroutine co_sum(x)
        integer :: x
        integer, save :: t[*]
        integer :: i

        t = x

        sync all

        if (this_image() == 1) then
          do i = 2, num_images()
              t = t + t[i]
          end do
        end if

        sync all

        x = t[1]

    end subroutine

end module


program DHT
  use initialise
  use co_helper
  integer :: i,j
  integer*8 :: pb,b,v
  integer :: start_wtime, end_wtime, rate
  integer :: M

  call inithash()

  call system_clock(start_wtime, rate)

  M = NHASH / 100

  do j=1,2
     call fresetvalue(numi,inum)
#ifdef PROGRESS_BAR
     if (this_image() == 1) then
         write (*,"('Pass ', i0,': ')", advance="no") j
     end if
#endif
     do i=1,NHASH
        call fnew(pb,b,v,numi)
#ifndef USE_AMO
        call hashlookup(b,v)
#else
        call hashlookup_with_amo(b,v)
#endif

#ifdef PROGRESS_BAR
        if (this_image()==1 .and. mod(i,M)==1) then
          write (*,"('.')",advance="no")
          call flush(OUTPUT_UNIT)
        end if
#endif
    end do
#ifdef PROGRESS_BAR
     if (this_image() == 1) then
         write (*,*) " DONE!"
     end if
#endif
     sync all
  end do

  sync all

  call system_clock(end_wtime)

  if(inum.eq.1 .and. NHASH .lt. 1000) then

     do j=1,numi
        do i=1,localhashlen
           if(hashcount(i)[j].gt.0) then
              write(*,*) j,i,hashtab(i)[j],hashcount(i)[j]
           end if
        end do
     end do
  end if

  call CO_SUM(collisions)
  if (inum .eq. 1) then
    write (*, '("Avg # collisions: ", F12.2)') &
          collisions /(1.0* num_images())
    write (*, '("Total time is ", F8.3," sec")') &
        (end_wtime-start_wtime)/(1.0*rate)
  end if

  call finhash()

end program DHT


