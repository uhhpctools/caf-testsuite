
c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine exchange_3(g,iex)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      use coarray_globals, only : buf, buf1

c---------------------------------------------------------------------
c   compute the right hand side based on exact solution
c---------------------------------------------------------------------

      implicit none

      include 'cafnpb.h'
      include 'applu.incl'

c---------------------------------------------------------------------
c  input parameters
c---------------------------------------------------------------------
      double precision  g(5,-1:isiz1+2,-1:isiz2+2,isiz3)
      integer iex

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer i, j, k
      integer ipos1, ipos2


      if (iex.eq.0) then
c---------------------------------------------------------------------
c   communicate in the south and north directions
c---------------------------------------------------------------------
      if (north.ne.-1) then
          sync images( north+1 )
      end if

c---------------------------------------------------------------------
c   send south
c---------------------------------------------------------------------
      if (south.ne.-1) then
          do k = 1,nz
            do j = 1,ny
              ipos1 = (k-1)*ny + j
              ipos2 = ipos1 + ny*nz
              buf(1,ipos1) = g(1,nx-1,j,k) 
              buf(2,ipos1) = g(2,nx-1,j,k) 
              buf(3,ipos1) = g(3,nx-1,j,k) 
              buf(4,ipos1) = g(4,nx-1,j,k) 
              buf(5,ipos1) = g(5,nx-1,j,k) 
              buf(1,ipos2) = g(1,nx,j,k)
              buf(2,ipos2) = g(2,nx,j,k)
              buf(3,ipos2) = g(3,nx,j,k)
              buf(4,ipos2) = g(4,nx,j,k)
              buf(5,ipos2) = g(5,nx,j,k)
            end do
          end do

          sync images( south+1 )
          buf1(1:5, 1:2*ny*nz)[south] = buf(1:5, 1:2*ny*nz)
          sync images( south+1 )
        end if

c---------------------------------------------------------------------
c   receive from north
c---------------------------------------------------------------------
        if (north.ne.-1) then

          sync images( north+1 )

          do k = 1,nz
            do j = 1,ny
              ipos1 = (k-1)*ny + j
              ipos2 = ipos1 + ny*nz
              g(1,-1,j,k) = buf1(1,ipos1)
              g(2,-1,j,k) = buf1(2,ipos1)
              g(3,-1,j,k) = buf1(3,ipos1)
              g(4,-1,j,k) = buf1(4,ipos1)
              g(5,-1,j,k) = buf1(5,ipos1)
              g(1,0,j,k) = buf1(1,ipos2)
              g(2,0,j,k) = buf1(2,ipos2)
              g(3,0,j,k) = buf1(3,ipos2)
              g(4,0,j,k) = buf1(4,ipos2)
              g(5,0,j,k) = buf1(5,ipos2)
            end do
          end do

        end if


      if (south.ne.-1) then
          sync images( south+1 )
      end if

c---------------------------------------------------------------------
c   send north
c---------------------------------------------------------------------
        if (north.ne.-1) then
          do k = 1,nz
            do j = 1,ny
              ipos1 = (k-1)*ny + j
              ipos2 = ipos1 + ny*nz
              buf(1,ipos1) = g(1,2,j,k)
              buf(2,ipos1) = g(2,2,j,k)
              buf(3,ipos1) = g(3,2,j,k)
              buf(4,ipos1) = g(4,2,j,k)
              buf(5,ipos1) = g(5,2,j,k)
              buf(1,ipos2) = g(1,1,j,k)
              buf(2,ipos2) = g(2,1,j,k)
              buf(3,ipos2) = g(3,1,j,k)
              buf(4,ipos2) = g(4,1,j,k)
              buf(5,ipos2) = g(5,1,j,k)
            end do
          end do

          sync images( north+1 )
          buf1(1:5, 1:2*ny*nz)[north] = buf(1:5, 1:2*ny*nz)
          sync images( north+1 )
        end if

c---------------------------------------------------------------------
c   receive from south
c---------------------------------------------------------------------
        if (south.ne.-1) then

          sync images( south+1 )

          do k = 1,nz
            do j = 1,ny
              ipos1 = (k-1)*ny + j
              ipos2 = ipos1 + ny*nz
              g(1,nx+2,j,k)  = buf1(1,ipos1)
              g(2,nx+2,j,k)  = buf1(2,ipos1)
              g(3,nx+2,j,k)  = buf1(3,ipos1)
              g(4,nx+2,j,k)  = buf1(4,ipos1)
              g(5,nx+2,j,k)  = buf1(5,ipos1)
              g(1,nx+1,j,k) = buf1(1,ipos2)
              g(2,nx+1,j,k) = buf1(2,ipos2)
              g(3,nx+1,j,k) = buf1(3,ipos2)
              g(4,nx+1,j,k) = buf1(4,ipos2)
              g(5,nx+1,j,k) = buf1(5,ipos2)
            end do
          end do
        end if

      else

c---------------------------------------------------------------------
c   communicate in the east and west directions
c---------------------------------------------------------------------
      if (west.ne.-1) then
          sync images( west+1 )
      end if

c---------------------------------------------------------------------
c   send east
c---------------------------------------------------------------------
        if (east.ne.-1) then
          do k = 1,nz
            do i = 1,nx
              ipos1 = (k-1)*nx + i
              ipos2 = ipos1 + nx*nz
              buf(1,ipos1) = g(1,i,ny-1,k)
              buf(2,ipos1) = g(2,i,ny-1,k)
              buf(3,ipos1) = g(3,i,ny-1,k)
              buf(4,ipos1) = g(4,i,ny-1,k)
              buf(5,ipos1) = g(5,i,ny-1,k)
              buf(1,ipos2) = g(1,i,ny,k)
              buf(2,ipos2) = g(2,i,ny,k)
              buf(3,ipos2) = g(3,i,ny,k)
              buf(4,ipos2) = g(4,i,ny,k)
              buf(5,ipos2) = g(5,i,ny,k)
            end do
          end do

          sync images( east+1 )
          buf1(1:5, 1:2*nx*nz)[east] = buf(1:5, 1:2*nx*nz)
          sync images( east+1 )
        end if

c---------------------------------------------------------------------
c   receive from west
c---------------------------------------------------------------------
        if (west.ne.-1) then

          sync images( west+1 )

          do k = 1,nz
            do i = 1,nx
              ipos1 = (k-1)*nx + i
              ipos2 = ipos1 + nx*nz
              g(1,i,-1,k) = buf1(1,ipos1)
              g(2,i,-1,k) = buf1(2,ipos1)
              g(3,i,-1,k) = buf1(3,ipos1)
              g(4,i,-1,k) = buf1(4,ipos1)
              g(5,i,-1,k) = buf1(5,ipos1)
              g(1,i,0,k) = buf1(1,ipos2)
              g(2,i,0,k) = buf1(2,ipos2)
              g(3,i,0,k) = buf1(3,ipos2)
              g(4,i,0,k) = buf1(4,ipos2)
              g(5,i,0,k) = buf1(5,ipos2)
            end do
          end do

        end if

      if (east.ne.-1) then
          sync images( east+1 )
      end if

c---------------------------------------------------------------------
c   send west
c---------------------------------------------------------------------
      if (west.ne.-1) then
          do k = 1,nz
            do i = 1,nx
              ipos1 = (k-1)*nx + i
              ipos2 = ipos1 + nx*nz
              buf(1,ipos1) = g(1,i,2,k)
              buf(2,ipos1) = g(2,i,2,k)
              buf(3,ipos1) = g(3,i,2,k)
              buf(4,ipos1) = g(4,i,2,k)
              buf(5,ipos1) = g(5,i,2,k)
              buf(1,ipos2) = g(1,i,1,k)
              buf(2,ipos2) = g(2,i,1,k)
              buf(3,ipos2) = g(3,i,1,k)
              buf(4,ipos2) = g(4,i,1,k)
              buf(5,ipos2) = g(5,i,1,k)
            end do
          end do


          sync images( west+1 )
          buf1(1:5, 1:2*nx*nz)[west] = buf(1:5, 1:2*nx*nz)
          sync images( west+1 )

        end if

c---------------------------------------------------------------------
c   receive from east
c---------------------------------------------------------------------
        if (east.ne.-1) then

          sync images( east+1 )

          do k = 1,nz
            do i = 1,nx
              ipos1 = (k-1)*nx + i
              ipos2 = ipos1 + nx*nz
              g(1,i,ny+2,k)  = buf1(1,ipos1)
              g(2,i,ny+2,k)  = buf1(2,ipos1)
              g(3,i,ny+2,k)  = buf1(3,ipos1)
              g(4,i,ny+2,k)  = buf1(4,ipos1)
              g(5,i,ny+2,k)  = buf1(5,ipos1)
              g(1,i,ny+1,k) = buf1(1,ipos2)
              g(2,i,ny+1,k) = buf1(2,ipos2)
              g(3,i,ny+1,k) = buf1(3,ipos2)
              g(4,i,ny+1,k) = buf1(4,ipos2)
              g(5,i,ny+1,k) = buf1(5,ipos2)
            end do
          end do

        end if

      end if

      return
      end     
