
c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine exchange_6(g,jbeg,jfin1)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      use coarray_globals, only : dum

c---------------------------------------------------------------------
c   compute the right hand side based on exact solution
c---------------------------------------------------------------------

      implicit none

      include 'cafnpb.h'
      include 'applu.incl'

c---------------------------------------------------------------------
c  input parameters
c---------------------------------------------------------------------
      double precision  g(0:isiz2+1,0:isiz3+1)
      integer jbeg, jfin1

c---------------------------------------------------------------------
c  local parameters
c---------------------------------------------------------------------
      integer k



c---------------------------------------------------------------------
c   communicate in the east and west directions
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   receive from east
c---------------------------------------------------------------------
      if (jfin1.eq.ny) then

        sync images( east+1 )
        sync images( east+1 )

        do k = 1,nz
          g(ny+1,k) = dum(k)
        end do

      end if

c---------------------------------------------------------------------
c   send west
c---------------------------------------------------------------------
      if (jbeg.eq.1) then
        do k = 1,nz
          dum(k) = g(1,k)
        end do

      sync images( west+1 )
      dum(1:nz)[west] = dum(1:nz)
      sync images( west+1 )

      end if

      return
      end     