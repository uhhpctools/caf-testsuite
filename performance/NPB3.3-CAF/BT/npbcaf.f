      module npbcaf

c---------------------------------------------------------------------
c The following include file is generated automatically by the
c "setparams" utility. It defines 
c      maxcells:      the square root of the maximum number of processors
c      problem_size:  12, 64, 102, 162 (for class T, A, B, C)
c      dt_default:    default time step for this problem size if no
c                     config file
c      niter_default: default number of iterations for this problem size
c---------------------------------------------------------------------

      include 'npbparams.h'

      integer, parameter :: maxcelldim = (problem_size/maxcells)+1,
     >      BUF_SIZE=maxcelldim*maxcelldim*(maxcells-1)*60+1,
     >      BUF_SIZZ=(2*maxcells-2)/maxcells*maxcelldim*maxcelldim*30+1

      integer, allocatable :: team(:), sync_succ(:,:), sync_pred(:,:)
      integer :: node, no_nodes, total_nodes, root
      integer :: comm_setup, comm_solve, comm_rhs, dp_type
      logical :: active

      double precision :: in_buffer(BUF_SIZE)
      integer :: buffoff(3), buff_id

      double precision :: out_buffer(BUF_SIZE+BUF_SIZZ*3)[*]
      double precision :: sh_rbuf(2)[*]
      integer :: sh_ibuf(9)[*]

      end module npbcaf
