
!---------------------------------------------------------------------
!
!  This program is a Bandwidth benchmark with CAF get statement used 
!  for the reduction.
!
!  For the output files you need to specify : 
!      the path, 
!      the communication layer,
!      the cluster name,
!      cores number and processes number respectively.
!
!
!
!  Name                Date            Notes
!  --------------      ----------      --------------------
!  Asma Farjallah      July 2010       Init
!  Siddhartha Jana     October 2013    Minor changes - Ready for test suite, removed file I/O 
!
!---------------------------------------------------------------------

program reduce

  implicit none

  integer,parameter   :: nt=(256*1024*1024)/4 
  integer,parameter   :: iterations=NITER

  integer             :: msg(nt)[*]
  integer,allocatable :: msg_sum(:)
  integer             :: me,ne
  integer             :: msg_size
  integer             :: i,k,im,it
  integer             :: ierr

  real(kind=8)        :: r_msgsize,r_iterations

  integer(kind=8)     :: srtc,ertc,res
  real(kind=8)        :: rtc,rtmp

  character(len=256)  :: output
  character(len=64)   :: suffix,path,layer,cluster,ncore,nproc

  me=this_image()
  ne=num_images()

  call getarg(1,path)
  call getarg(2,layer)
  call getarg(3,cluster)
  call getarg(4,ncore)
  call getarg(5,nproc)

  suffix=trim(layer)//"_"//trim(cluster)//&
       "_NC"//trim(ncore)//"_NP"//trim(nproc)//".dat"

  output=trim(path)//"/reduce_CAF_"//suffix

  if (me == 1) then
     write(*,'(A1,A10,A21,A20)') "#","[Bytes]","[Microsec]","[KB/sec]"
  endif

  allocate(msg_sum(nt))

  i=1 
  msg(:)=1
  msg_size=1

  do while (msg_size <= nt)

     msg_sum(:)=0

     sync all

     if (me == 1) then

        write(*,'(I2)',advance='no') i

        call get_rtc(srtc)

        do it=1,iterations
           do im=1,ne
              msg_sum(1:msg_size)=msg_sum(1:msg_size)+msg(1:msg_size)[im]
           enddo
        enddo

        call get_rtc(ertc)
        call get_rtc_res(res)
        rtmp=res
        rtc=(ertc-srtc)/rtmp

        r_msgsize=msg_size
        r_iterations=iterations

        write(*,'(I10,E20.8,E20.8)') 4*msg_size,rtc*1000000.0/r_iterations,4.0*r_msgsize*r_iterations/rtc/1024.0 !!KB/sec

        do k=1,msg_size
           if (msg_sum(k) /= ne*iterations) then
              write(*,*) "Data received is incomplete."
              call EXIT(1)
           endif
        enddo

     endif

     sync all

     i=i+1
     msg_size=2*msg_size

  enddo

  deallocate(msg_sum)

end program reduce
