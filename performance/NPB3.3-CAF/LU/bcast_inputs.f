c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine bcast_inputs

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      use coarray_globals, only : sh_ibuf, sh_dpbuf

      implicit none

      include 'cafnpb.h'
      include 'applu.incl'

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer ierr

c---------------------------------------------------------------------
c   root broadcasts the data
c   Using naive broadcast here, since its just done once.
c---------------------------------------------------------------------

      if (id .eq. root .and. num .ne. 1) then
          sh_ibuf(1) = ipr
          sh_ibuf(2) = inorm
          sh_ibuf(3) = itmax
          sh_ibuf(4) = nx0
          sh_ibuf(5) = ny0
          sh_ibuf(6) = nz0

          sh_dpbuf(1) = dt
          sh_dpbuf(2) = omega
          sh_dpbuf(3:7) = tolrsd(1:5)
      end if

      sync all

      if (id .ne. root) then
          sh_ibuf(1:6) = sh_ibuf(1:6)[root]
          sh_dpbuf(1:6) = sh_dpbuf(1:6)[root]

          ipr = sh_ibuf(1)
          inorm = sh_ibuf(2)
          itmax = sh_ibuf(3)
          nx0 = sh_ibuf(4)
          ny0 = sh_ibuf(5)
          nz0 = sh_ibuf(6)

          dt = sh_dpbuf(1)
          omega = sh_dpbuf(2)
          tolrsd(1:5) = sh_dpbuf(3:7)
      end if

      return
      end


