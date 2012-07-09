!---------------------------------------------------------------------
!
!  This program is Round-Trip benchmark with CAF put and get
!  statements.
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
!  Debjyoti Majumder   July 2011       Minor change
!  Siddhartha Jana     October 2013    Minor changes - Ready for test suite, removed file I/O 
!
!---------------------------------------------------------------------

program bidirectional

  implicit none

  integer,parameter :: nt=1024*1024
  integer,parameter :: iterations=NITER

  integer,allocatable::msg1(:)[:],msg2(:)[:]
  integer           :: me
  integer           :: msg_size
  integer           :: i,j,k,it
  integer           :: ierr

  real(kind=8)      :: r_msgsize,r_iterations

  integer(kind=8)   :: srtc,ertc,res
  real(kind=8)      :: rtc,rtmp

  character(len=512):: output
  character(len=256) :: suffix,path
  character(len=64) :: layer,cluster,ncore,nproc

  allocate(msg1(nt)[*],msg2(nt)[*])

  me=this_image()
  if (num_images() ==  1) then
     print *, "number of images can not be 1"
     call exit(0)
  end if


  if (me == 1) then
     write(* ,'(A1,A10,A21,A20)') "#","[Bytes]","[Microsec]","[KB/sec]"
  endif

  i=1
  msg1(:)=0; msg2(:)=0
  msg_size=1

  do while (msg_size < nt)

    if (me == 1) then
        write(*,'(I2)',advance='no') i
    end if

    sync all

    call get_rtc(srtc)

    do it=1,iterations
        if (me==1) then
           msg1(1:msg_size)[2]=msg1(1:msg_size)
        else if (me==2) then
           msg2(1:msg_size)[1]=msg2(1:msg_size)
        endif
    enddo

    sync all

    if (me==1) then
        call get_rtc(ertc)
        call get_rtc_res(res)
        rtmp=res

        rtc=(ertc-srtc)/rtmp

        r_msgsize=msg_size*2
        r_iterations=iterations

        write(*,'(I10,E20.8,E20.8)') 4*msg_size,rtc*1000000.0/r_iterations,4.0*r_msgsize*r_iterations/rtc/1024.0

        i=i+1

     endif

     msg_size=2*msg_size

  enddo

  deallocate (msg1, msg2)

end program bidirectional
