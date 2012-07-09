      module coarray_globals
          private

          public :: buff, sh_ibuf

          ! from npbparams.h and globals.h
          integer nm, nm2, lm
          parameter( lm=7)
          parameter( nm=2+2**lm )
          parameter( nm2=2*nm*nm )

          double precision :: buff(nm2,4)[0:*]

          integer          :: sh_ibuf(13)[0:*]

      end module
