! checking for correctness of num_images()

      program main
            implicit none

            integer, parameter :: N=NPROCS
            integer :: np

            np = num_images()

            if (np /= N) then
              call EXIT(1)
            end if

        end program main

