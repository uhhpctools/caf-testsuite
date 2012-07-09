      module coarray_globals
          private

          public :: buf, buf1, dum, sh_ibuf, sh_dpbuf

          include 'npbparams.h'

          double precision buf(5,2*isiz2*isiz3)[0:*]
          double precision buf1(5,2*isiz2*isiz3)[0:*]
          double precision dum(1024)[0:*]

          integer          sh_ibuf(6)[0:*]
          double precision sh_dpbuf(7)[0:*]
      end module
