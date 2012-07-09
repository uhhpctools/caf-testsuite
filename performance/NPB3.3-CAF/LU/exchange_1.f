
c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine exchange_1( g,k,iex )

c---------------------------------------------------------------------
c---------------------------------------------------------------------


      implicit none

      include 'cafnpb.h'
      include 'applu.incl'

      double precision  g(5,-1:isiz1+2,-1:isiz2+2,isiz3)
      integer k
      integer iex
      integer i, j
      double precision :: dum(5,isiz1+isiz2)
      double precision, save :: dum1(5,isiz1+isiz2)[0:*]



      if( iex .eq. 0 ) then

          if( north .ne. -1 ) then

              sync images( north+1 )
              sync images( north+1 )

              do j=jst,jend
                  g(1,0,j,k) = dum1(1,j)
                  g(2,0,j,k) = dum1(2,j)
                  g(3,0,j,k) = dum1(3,j)
                  g(4,0,j,k) = dum1(4,j)
                  g(5,0,j,k) = dum1(5,j)
              enddo
          endif

          if( west .ne. -1 ) then

              sync images( west+1 )
              sync images( west+1 )

              do i=ist,iend
                  g(1,i,0,k) = dum1(1,i)
                  g(2,i,0,k) = dum1(2,i)
                  g(3,i,0,k) = dum1(3,i)
                  g(4,i,0,k) = dum1(4,i)
                  g(5,i,0,k) = dum1(5,i)
              enddo
          endif

      else if( iex .eq. 1 ) then

          if( south .ne. -1 ) then

              sync images( south+1 )
              sync images( south+1 )

              do j=jst,jend
                  g(1,nx+1,j,k) = dum1(1,j)
                  g(2,nx+1,j,k) = dum1(2,j)
                  g(3,nx+1,j,k) = dum1(3,j)
                  g(4,nx+1,j,k) = dum1(4,j)
                  g(5,nx+1,j,k) = dum1(5,j)
              enddo
          endif

          if( east .ne. -1 ) then

              sync images( east+1 )
              sync images( east+1 )

              do i=ist,iend
                  g(1,i,ny+1,k) = dum1(1,i)
                  g(2,i,ny+1,k) = dum1(2,i)
                  g(3,i,ny+1,k) = dum1(3,i)
                  g(4,i,ny+1,k) = dum1(4,i)
                  g(5,i,ny+1,k) = dum1(5,i)
              enddo
          endif

      else if( iex .eq. 2 ) then

          if( south .ne. -1 ) then
              do j=jst,jend
                  dum(1,j) = g(1,nx,j,k) 
                  dum(2,j) = g(2,nx,j,k) 
                  dum(3,j) = g(3,nx,j,k) 
                  dum(4,j) = g(4,nx,j,k) 
                  dum(5,j) = g(5,nx,j,k) 
              enddo

              sync images( south+1 )
              dum1(1:5,jst:jend)[south] = dum(1:5,jst:jend)
              sync images( south+1 )

          endif

          if( east .ne. -1 ) then
              do i=ist,iend
                  dum(1,i) = g(1,i,ny,k)
                  dum(2,i) = g(2,i,ny,k)
                  dum(3,i) = g(3,i,ny,k)
                  dum(4,i) = g(4,i,ny,k)
                  dum(5,i) = g(5,i,ny,k)
              enddo

              sync images( east+1 )
              dum1(1:5,ist:iend)[east] = dum(1:5,ist:iend)
              sync images( east+1 )
          endif

      else

          if( north .ne. -1 ) then
              do j=jst,jend
                  dum(1,j) = g(1,1,j,k)
                  dum(2,j) = g(2,1,j,k)
                  dum(3,j) = g(3,1,j,k)
                  dum(4,j) = g(4,1,j,k)
                  dum(5,j) = g(5,1,j,k)
              enddo

              sync images( north+1 )
              dum1(1:5,jst:jend)[north] = dum(1:5,jst:jend)
              sync images( north+1 )
          endif

          if( west .ne. -1 ) then
              do i=ist,iend
                  dum(1,i) = g(1,i,1,k)
                  dum(2,i) = g(2,i,1,k)
                  dum(3,i) = g(3,i,1,k)
                  dum(4,i) = g(4,i,1,k)
                  dum(5,i) = g(5,i,1,k)
              enddo

              sync images( west+1 )
              dum1(1:5,ist:iend)[west] = dum(1:5,ist:iend)
              sync images( west+1 )
          endif

      endif

      end



