
c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine init_comm

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c
c   establish rank and size
c
c This is a module in the CAF  implementation of LUSSOR
c pseudo application from the NAS Parallel Benchmarks.
c
c---------------------------------------------------------------------

      implicit none

      include 'cafnpb.h'
      include 'applu.incl'

      integer nodedim

c---------------------------------------------------------------------
c   establish the global rank of this process
c---------------------------------------------------------------------

      id = this_image() - 1

c---------------------------------------------------------------------
c   establish the size of the global group
c---------------------------------------------------------------------

      num = num_images()

      ndim   = nodedim(num)

      return
      end
