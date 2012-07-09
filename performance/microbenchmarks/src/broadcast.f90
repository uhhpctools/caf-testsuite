
!---------------------------------------------------------------------
!
!  This program is a Bandwidth benchmark with CAF put statement used 
!  for the broadcast.
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
!  Debjyoti Majumder   July 2011       Minor modifications
!  Siddhartha Jana     October 2013    Minor changes - Ready for test suite, removed file I/O 
!
!---------------------------------------------------------------------

program broadcast

  implicit none

  integer,parameter :: nt=1024*1024
  integer,parameter :: iterations=NITER

  integer, allocatable:: msg(:)[:]
  integer           :: me,ne
  integer           :: msg_size
  integer           :: i,k,im,it
  integer           :: ierr

  real(kind=8)      :: r_msgsize,r_iterations

  integer(kind=8)   :: srtc,ertc,res
  real(kind=8)      :: rtc,rtmp

  character(len=512):: output
  character(len=256) :: suffix,path
  character(len=64) :: layer,cluster,ncore,nproc

  allocate ( msg(nt)[*] )

  me=this_image()
  ne=num_images()

  call getarg(1,path)
  call getarg(2,layer)
  call getarg(3,cluster)
  call getarg(4,ncore)
  call getarg(5,nproc)

  suffix=trim(layer)//"_"//trim(cluster)//&
       "_NC"//trim(ncore)//"_NP"//trim(nproc)//".dat"

  output=trim(path)//"/broadcast_CAF_"//suffix

  if (me == 1) then
     write(*,'(A1,A10,A21,A20)') "#","[Bytes]","[Microsec]","[KB/sec]"
  endif

  i=1
  msg(:)=me
  msg_size=1

  do while (msg_size < nt)

     sync all

     if (me == 1) then

        write(*,'(I2,A10,I10)') i," msg_size:",msg_size

        call get_rtc(srtc)

        do it=1,iterations
           do im=2,ne
              msg(1:msg_size)[im]=msg(1:msg_size)
           enddo
        enddo

        call get_rtc(ertc)
        call get_rtc_res(res)
        rtmp=res
        rtc=(ertc-srtc)/rtmp

        r_msgsize=msg_size
        r_iterations=iterations

        write(*,'(I10,A1,E20.8,A1,E20.8)') 4*msg_size,";",rtc*1000000.0/r_iterations,";",4.0*r_msgsize*r_iterations/rtc/1024.0 !!KB/sec

      endif

     sync all

     if (me /= 1) then

        do k=1,msg_size

           if (msg(k) /= 1) then
              write(*,*) "Data received is incomplete."
              call EXIT(1)
           endif

        enddo

     endif

     msg_size=2*msg_size
     i=i+1

  enddo
  
  deallocate ( msg )

  stop

end program broadcast
