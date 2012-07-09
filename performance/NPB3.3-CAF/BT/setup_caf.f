
c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine setup_caf

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c set up CAF stuff
c---------------------------------------------------------------------

      use npbcaf
      implicit none
      integer i, error, nc

      node = THIS_IMAGE()
      total_nodes = NUM_IMAGES()

c---------------------------------------------------------------------
c     compute square root; add small number to allow for roundoff
c---------------------------------------------------------------------
      nc = dint(dsqrt(dble(total_nodes) + 0.00001d0))

c---------------------------------------------------------------------
c We handle a non-square number of nodes by making the excess nodes
c inactive. However, we can never handle more cells than were compiled
c in. 
c---------------------------------------------------------------------

      if (nc .gt. maxcells) nc = maxcells

      if (node .gt. nc*nc) then
         active = .false.
      else
         active = .true.
      end if
      no_nodes = nc * nc

c---------------------------------------------------------------------
c     let node 1 be the root for the group (there is only one)
c---------------------------------------------------------------------
      root = 1
      if (no_nodes .ne. total_nodes) then
         if (node .eq. root) write(*,1000) total_nodes, no_nodes
1000     format(' Total number of nodes',I5,
     &          ' does not match with squared number of nodes',I5)
         stop
      endif
      if (.not.active) return

c---------------------------------------------------------------------
c     allocate space
c---------------------------------------------------------------------
      allocate(team(no_nodes), stat=error)
      do i = 1, no_nodes
         team(i) = i
      end do
      i = 1
      if (nc .gt. 2) i = 2
      allocate(sync_pred(i,3), stat=error)
      allocate(sync_succ(i,3), stat=error)

      buffoff(1) = BUF_SIZE
      buffoff(2) = buffoff(1) + BUF_SIZZ
      buffoff(3) = buffoff(2) + BUF_SIZZ
      buff_id = 1

      return
      end

