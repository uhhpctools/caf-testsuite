!==========================================================================
! Forward 2D : Variable z grid implementation.
!==========================================================================

      module cg_2d
       implicit none
      contains

         !========================================================================
         subroutine cg_fwd_inner_2d(                                    &
                                     im,ip,km,kp,                       &
                                     lx,lz,                             &
                                     xmin,xmax,zmin,zmax,               &
                                     coefx,coefz,                       &
                                     u,v,roc2                           &
                                    )
           !
           ! This subroutine updates the wavefield with no damping.
           !

           implicit none

           integer,intent(in) :: im,ip,km,kp
           integer,intent(in) :: lx,lz
           integer,intent(in) :: xmin,xmax,zmin,zmax
           real,intent(in)    :: coefx(0:lx)
           real,intent(in)    :: coefz(0:lz)
           real,intent(inout) :: u(xmin-lx:xmax+lx,1,zmin-lz:zmax+lz)
           real,intent(in)    :: v(xmin-lx:xmax+lx,1,zmin-lz:zmax+lz)
           real,intent(in)    :: roc2(xmin:xmax,1,zmin:zmax)

           integer            :: i,k
           real               :: lap,coef0
           coef0=coefx(0)+coefz(0)
           do k=km,kp
               do i=im,ip
                  !! BASIC.
                  lap=coef0*v(i,1,k)                                    &
                       +coefx(1)*(v(i+1,1,k)+v(i-1,1,k))                &
                       +coefz(1)*(v(i,1,k+1)+v(i,1,k-1))                &
                       +coefx(2)*(v(i+2,1,k)+v(i-2,1,k))                &
                       +coefz(2)*(v(i,1,k+2)+v(i,1,k-2))                &
                       +coefx(3)*(v(i+3,1,k)+v(i-3,1,k))                &
                       +coefz(3)*(v(i,1,k+3)+v(i,1,k-3))                &
                       +coefx(4)*(v(i+4,1,k)+v(i-4,1,k))                &
                       +coefz(4)*(v(i,1,k+4)+v(i,1,k-4))

                  !! Update the wavefield.
                  u(i,1,k)=2.*v(i,1,k)-u(i,1,k)+roc2(i,1,k)*lap
               enddo
           enddo
           return
         end subroutine cg_fwd_inner_2d
      end module


      !================================================== main program
      program cg_main

      use cg_2d
      implicit none

      integer           :: npx
      integer           :: px,pz,me,npz
      integer           :: i
#ifdef NITER
      integer, parameter:: lx=4,lz=4,nt=NITER
#else
      integer, parameter:: lx=4,lz=4,nt=1000
#endif
      !integer, parameter:: xmin=1,xmax=1000,zmin=1,zmax=500
      integer           :: xmin,xmax,zmin,zmax
      real,parameter    :: dx=4,dz=4,fmax=15,c=3000
      integer           :: it,xsource,zsource,l,z
      integer           :: x1,x2,x3,x4,x5,x6,z1,z2,z3,z4,z5
      integer           :: im,ip,km,kp
      integer           :: ix, iz
      integer           :: ub
      real              :: dt
      real              :: source (nt)
      real              :: coefx(0:lx),coefz(0:lz)
      real              :: hdx_2,hdz_2
      character(len=16) :: char
      real              :: rmax, rmin
      real, allocatable :: roc2(:,:)
      real, allocatable :: u(:,:)[:,:,:]
      real, allocatable :: v(:,:)[:,:,:]
      real              :: max_u[*], min_u[*]

      integer           :: nargs, args(4)[*]
      integer           :: P, Q, N, M
      character(len=20) :: buffer

      integer :: ticks, start_time, end_time, rate


      me = this_image()

      if (me == 1) then
          nargs = command_argument_count()
          if (nargs == 0) then
              write (*,*) "Conjugate Gradient Method with PML"
              write (*,*) "N      = 1st dimension of entire rectangular grid"
              write (*,*) "M      = 2nd dimension of entire rectangular grid"
              write (*,*) "P      = number of nodes in 1st dimension, N/P an integer"
              write (*,*) "Q      = number of nodes in 2nd dimension, N/Q an integer"
              write (*,*) "Total number of nodes = P*Q, must be NUM_IMAGES()"
              write (*,'(a)',advance='no') "Enter N, M, P, and Q: "
              read (5,*) args(1:4)
              !"
          else
            call getarg(1,buffer)
            read(buffer,*) args(1)
            call getarg(2,buffer)
            read(buffer,*) args(2)
            call getarg(3,buffer)
            read(buffer,*) args(3)
            call getarg(4,buffer)
            read(buffer,*) args(4)
          end if
      end if

      if (me == 1) then
          sync images(*)
      else
          sync images(1)
      end if

      N = args(1)[1]
      M = args(2)[1]
      P = args(3)[1]
      Q = args(4)[1]
      if ((N/P)*P /= N .or. (M/Q)*Q /= M .or. P*Q /= num_images()) then
          if (ME == 1) then
              write(6,*) 'BAD INPUT VALUES'
              call flush(6)
          end if
          sync all
          stop
      end if
      xmin = 1
      xmax = N/P
      zmin = 1
      zmax = M/Q
      npx  = P

      allocate (roc2(xmin:xmax, zmin:zmax))
      allocate (u(xmin-lx:xmax+lx, zmin-lz:zmax+lz)[npx,1,*])
      allocate (v(xmin-lx:xmax+lx, zmin-lz:zmax+lz)[npx,1,*])

      npz = num_images() / npx

      ! compute coefs
      call second_derivative_coef(coefx,lx)
      call second_derivative_coef(coefz,lz)
      coefx=coefx/dx/dx
      coefz=coefz/dz/dz

      ! set propagator coef
      dt=0.4*max(dx,dz)/c
      hdx_2=1./(4.*dx*dx)
      hdz_2=1./(4.*dz*dz)
      roc2=c*c*dt*dt

      ! initialize arrays
      source=0
      u=0.
      v=0.
      px = this_image(v,1)
      pz = this_image(v,3)

      x1 = xmin
      x6 = xmax
      if (px == 1) then
          x2 = xmin+4
          x3 = xmin+5
      else
          x2 = 0
          x3 = xmin
      end if
      if (px == npx) then
          x4 = xmax-5
          x5 = xmax-4
      else
          x4 = xmax
          x5 = xmax+1
      end if
      z3=zmin
      z4=zmax
      if (pz == 1) then
          z3 = 2
      end if

      ! computer source term
      if ( mod(npx,2) == 0) then
          xsource = xmax
          ix = npx/2
      else
          xsource = xmax/2
          ix = npx/2+1
      end if
      if ( mod(npz,2) == 0) then
          zsource = zmax
          iz = npz/2
      else
          zsource = zmax/2
          iz = npz/2+1
      endif

      call csource(nt,fmax,dt,source)

      im = xmin
      ip = xmax
      km = zmin
      kp = zmax
      if (px ==1) im = xmin+lx
      if (px ==npx) ip = xmax-lx
      if (pz ==1) km = zmin+lz

      ub = ucobound(v, 3)

      if (pz == ub ) kp = zmax-lz


      sync all

      call system_clock(start_time, rate)

      do it=1,nt,2
        max_u = maxval( u(xmin:xmax,zmin:zmax) )
        min_u = minval( u(xmin:xmax,zmin:zmax) )

        sync all

        if (me ==1) then
            do i=2,num_images()
                rmax = max_u[i]
                rmin = min_u[i]
                if (rmax > max_u) max_u = rmax
                if (rmin < min_u) min_u = rmin
            end do
            write (*,'(i4,f8.1,f8.1,f8.1,f8.1)')                        &
                   it, max_u, min_u, source(it), dt
        end if

        if (px == ix .and. pz == iz) then
            !print *, "image ", me, " getting source input for U"
            u(xsource, zsource) = u(xsource, zsource) + source(it)
        end if

        call cg_fwd_inner_2d(                                           &
                            im,ip,km,kp,                                &
                            lx,lz,                                      &
                            xmin,xmax,zmin,zmax,                        &
                            coefx,coefz,                                &
                            u,v,roc2                                    &
                            )

        sync all

        ! get data from top neighbor
        if( px>1) then
            u(xmin-lx:xmin-1,zmin:zmax) =                               &
                      u(xmax-lx+1:xmax,zmin:zmax)[px-1,1,pz]
        end if

        ! get data from bottom neighbor
        if (px<npx) then
            u(xmax+1:xmax+lx,zmin:zmax) =                               &
                      u(xmin:xmin+lx-1,zmin:zmax)[px+1,1,pz]
        endif

        ! get data from left neighbor
        if (pz > 1) then
            u(xmin:xmax,zmin-lz:zmin-1) =                               &
                      u(xmin:xmax,zmax-lz+1:zmax)[px,1,pz-1]
        end if

        ! get data from right neighbor
        ub = ucobound(u, 3)
        if (pz< ub) then
            u(xmin:xmax,zmax+1:zmax+lz) =                               &
                      u(xmin:xmax,zmin:zmin+lz-1)[px,1,pz+1]
        end if

        sync all

        if (px == ix .and. pz == iz) then
            !print *, "image ", me, " getting source input for V"
            v(xsource, zsource) = v(xsource, zsource) + source(it+1)
        end if

        call cg_fwd_inner_2d(                                           &
                            im,ip,km,kp,                                &
                            lx,lz,                                      &
                            xmin,xmax,zmin,zmax,                        &
                            coefx,coefz,                                &
                            v,u,roc2                                    &
                            )

        sync all

        ! get data from top neighbor
        if( px>1) then
            v(xmin-lx:xmin-1,zmin:zmax) =                               &
                      v(xmax-lx+1:xmax,zmin:zmax)[px-1,1,pz]
        end if

        ! get data from bottom neighbor
        if (px<npx) then
            v(xmax+1:xmax+lx,zmin:zmax) =                               &
                      v(xmin:xmin+lx-1,zmin:zmax)[px+1,1,pz]
        endif

        ! get data from left neighbor
        if (pz > 1) then
            v(xmin:xmax,zmin-lz:zmin-1) =                               &
                      v(xmin:xmax,zmax-lz+1:zmax)[px,1,pz-1]
        end if

        ! get data from right neighbor
        ub = ucobound(v, 3)
        if (pz< ub) then
            v(xmin:xmax,zmax+1:zmax+lz) =                               &
                      v(xmin:xmax,zmin:zmin+lz-1)[px,1,pz+1]
        end if

      enddo

      sync all

      call system_clock(end_time)

        if (me == 1) then
          ticks = end_time - start_time
          write(*, '(//A20,I8,A)')   "clock rate = ", rate, " ticks/s"
          write(*, '(A20,I8)')           "ticks  = ", ticks
          write(*, '(A20,F8.2,A)') "elapsed time = ", ticks/(1.0*rate), " seconds"
        end if


      ! TODO: support I/O for coarray data
!        if (this_image() == 1) then
!            open(1,file='snap.H',form='formatted')
!            write(1,*)'in=snap.H@'
!            write(char,*)(xmax-xmin+2*lx+1)
!            write(1,*)'n1='//trim(adjustl(char))
!            write(char,*)(zmax-zmin+2*lz+1)
!            write(1,*)'n2='//trim(adjustl(char))
!            write(char,*)1
!            write(1,*)'n3='//trim(adjustl(char))
!            write(char,*)dx
!            write(1,*)'d1='//trim(adjustl(char))
!            write(char,*)dz
!            write(1,*)'d2='//trim(adjustl(char))
!            write(1,*)'d3=1'
!            write(1,*)'o1=0'
!            write(1,*)'o2=0.'
!            write(1,*)'o3=0.'
!            write(1,*)'esize=4'
!            write(1,*)'data_format=native_float'
!            print*,'n1=',(xmax-xmin+2*lx+1)&
!                  ,'n2=',(zmax-zmin+2*lz+1)
!            close(1)
!            open(1,file='snap.H@',access='direct',&
!                   recl=4*(xmax-xmin+2*lx+1)&
!                         *(zmax-zmin+2*lz+1))
!            write(1,rec=1)u
!            close(1)
!        end if

! 'stop' in the next stmt has been commented since G95 does not exit images cleanly.
      !stop

      contains
      !===================================================================
      subroutine second_derivative_coef(coef,l)
      !
      ! Given the half-order l of desired stencil,this routine returns
      ! coef which contains the standard associated FD stencil.
      !
      implicit none
      integer,intent(in) :: l
      real,intent(out)   :: coef(0:l)
      select case(l)
         case(0)
            coef=(/             0./)
         case(1)
            coef=(/            -2.,    1./)
         case(2)
            coef=(/         -5./2., 4./3., -1./12./)
         case(3)
            coef=(/       -49./18., 3./2., -3./20.,  1./90./)
         case(4)
            coef=(/      -205./72., 8./5.,  -1./5., 8./315., &
                 -1./560./)
         case(5)
            coef=(/   -5269./1800., 5./3., -5./21., 5./126., &
                 -5./1008.,  1./3150./)
         case(6)
            coef=(/   -5369./1800.,12./7.,-15./56.,10./189., &
                 -1./112.,  2./1925.,  1./16632./)
         case(7)
            coef=(/-266681./88200., 7./4., -7./24., 7./108., &
                 -7./528.,  7./3300., -7./30888.,  1./84084./)

         case default
            write(6,*) 'Error ! Standard FD stencil : invalid order.'
            write(6,*) 'Notice that 2 <= order <= 14.'
            call flush(6)
            error stop
      end select
      return
      end subroutine second_derivative_coef
      ! ======================================
      subroutine csource(nt,fpeak,dt,source)
       !--
       implicit none
       integer   :: it,nt
       real      :: fpeak,dt,t
       real      :: pi,tpeak,lam
       real      :: source(nt)
       !--
       tpeak=0
       pi=3.1415927
       lam=pi*pi*fpeak*fpeak

       do it=1,nt
          t=dt*real(it-1)
          source (it)=2.*lam*(2.*lam*(t-tpeak)*(t-tpeak)-1)*exp(-lam*&
          (t-tpeak)**2)

       enddo
       return
       end subroutine csource
      end program cg_main
