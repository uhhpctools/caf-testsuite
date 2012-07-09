

      SUBROUTINE gray_erode(img, height, filterheight, filterwidth, &
            & iterations, pixrange, wLbound, wUbound, localWidth)
        IMPLICIT NONE
        integer, intent(in) :: height, filterheight, filterwidth
        integer, intent(in) :: wLbound, wUbound, localWidth
        integer, intent(in) :: pixrange, iterations
        integer, intent(inout) :: img(height,512)[*]

        integer :: ghostCells(2048,4)
        integer :: maxlabel, ghostImage
        integer :: fh, fw, iters, pixval, i, j, s, imgRow, imgCol, labelindex
        integer :: fhlimit
        integer :: fwlimit
        integer :: smoothedlabels(height,localWidth)
        integer :: labeltemp(pixrange)
        integer :: loopStart, loopEnd, thisImage, imageCount


        maxlabel = 0
        pixval = 0
        fhlimit = filterheight/2
        fwlimit = filterwidth/2

        smoothedlabels(:,1:512)=img(:,1:512) !copy

        labeltemp=0
        thisImage = this_image()
        imageCount = num_images()

        if (thisImage==imageCount) then
            loopEnd=localWidth-fwlimit
        else
            loopEnd=localWidth
        end if

        if (thisImage== 1) then
            loopStart=1+fwlimit
        else
            loopStart=1
        end if


        ITER: DO iters = 1,iterations

        sync all
        if(thisImage==1) then
          ghostCells(:,3:4)=img(:,1:2)[thisImage+1]
        else if(thisImage==imageCount) then
          ghostCells(:,1:2)=img(:,511:512)[thisImage-1]
        else
          ghostCells(:,3:4)=img(:,1:2)[thisImage+1]
          ghostCells(:,1:2)=img(:,511:512)[thisImage-1]
        end if

            J_WIDTH: DO j = loopStart , loopEnd
                I_HEIGHT: DO i = fhlimit+1, height-fhlimit

                  FW_EACH: DO fw = -fwlimit, fwlimit
                    FH_EACH: DO fh = -fhlimit,fhlimit
                        imgRow=i+fh
                        imgCol=j+fw
                        if(imgCol<1) then
                            labelindex=(ghostCells(imgRow,imgCol+fwlimit))+1!To avoid 0
                        else if (imgCol>512) then
                            labelindex=(ghostCells(imgRow,imgCol-512+fwlimit))+1!To avoid 0
                        else
                            labelindex=(img(imgRow,imgCol))+1!To avoid 0
                        end if
                        labeltemp(labelindex)=labeltemp(labelindex)+1!increment
                    END DO FH_EACH
                  END DO FW_EACH

                  FINDMAX: DO s = 1,pixrange
                    if (labeltemp(s)>maxlabel) then
                        maxlabel=labeltemp(s)
                        pixval = s-1
                    end if
                  END DO FINDMAX

                  smoothedlabels(i,j)=pixval

                  labeltemp=0
                  maxlabel=0

                END DO I_HEIGHT
            END DO J_WIDTH

            img(:,1:512)=smoothedlabels(:,1:512) !copy back

        END DO ITER


    END SUBROUTINE gray_erode


    PROGRAM smoothing_main
        IMPLICIT NONE

      INTERFACE
      SUBROUTINE gray_erode(img, height, filterheight, filterwidth, &
            & iterations, pixrange, wLbound, wUbound, localWidth)
        IMPLICIT NONE
        integer, intent(in) :: height, filterheight, filterwidth
        integer, intent(in) :: wLbound, wUbound, localWidth
        integer, intent(in) :: pixrange, iterations
        integer, intent(inout) :: img(height,512)[*]

      END SUBROUTINE gray_erode
      END INTERFACE


        integer, parameter :: height=2048, width=2048
        integer, parameter :: filterheight=5, filterwidth=5
        integer, parameter :: pixrange=5, iterations=10
        real :: t_start, t_stop
        integer :: image_count=4, thisImage !Change here
        integer :: rec_index, wUbound=width, wLbound, localWidth, columnIndex=1
        integer :: labels(2048,512)[*]!Change here
        integer :: ticks, start_time, end_time, rate

        thisImage = this_image()

        localWidth=width/image_count
        wUbound=thisImage*localWidth
        wLbound=wUbound-localWidth+1

        if (image_count/=num_images()) then
          print *,"Number of Images must be ",image_count
          stop
        end if

        print *, "labels(100:200, 10) = ", labels(100:200,10)


         ! READ IMAGE FROM FILE
         open (1, file='input_data/inlabels_2048.bdf', FORM='unformatted', &
         & access='direct', recl=height*sizeof(height))

         do rec_index=wLbound,wUbound
             read (1, rec=rec_index) labels(:,columnIndex)
             columnIndex=columnIndex+1
         end do
         close(1)
         !END READ IMAGE FROM FILE

        !DO SMOOTHING
        sync all
        call system_clock(start_time, rate)
        call gray_erode(labels, height, filterheight, filterwidth, &
          & iterations, pixrange, wLbound, wUbound, localWidth)
        sync all
        call system_clock(end_time)

        print '("Image:",i2,"-> Time = ",f6.3," seconds.")',this_image(),t_stop - t_start
        !END DO SMOOTHING


      if (thisImage == 1) then
         !WRITE IMAGE TO FILE
         columnIndex=1
         open (2, file='output_data/outlabels_2048.bdf', FORM='unformatted',&
         & access='direct', recl=height*sizeof(height))!, status='replace')
         do rec_index=wLbound,wUbound
             write (2, rec=rec_index) labels(:,columnIndex)
             columnIndex=columnIndex+1
         end do
         close(2)
         !END OF WRITE IMAGE TO FILE

        ticks = end_time - start_time
        write(*, '(//A20,I8,A)')   "clock rate = ", rate, " ticks/s"
        write(*, '(A20,I8)')           "ticks  = ", ticks
        write(*, '(A20,F8.2,A)') "elapsed time = ", ticks/(1.0*rate), " seconds"
      end if

    END PROGRAM smoothing_main
