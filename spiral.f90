program spiral


  integer(8) sn
  integer(8) n, j, k, sn
  character(:) aline
  do
     print*, 'enter n'
     read(*,*) n
     print*, 'enter parameters'
     read(*,*) aline
     read(aline, '(i, i)', iostat=ios) j, k
     if(ios<0) then
        sn = shellnum(n/2,j)
        os  = shellnum(n)
        write(*, *) os
     end do
   end program

   !       +---+
   !       |   |
   !       | *-+
   !       +-

   subroutine spiral_to_location(n, j, x, y)
     integer(8):: n,  & ! the size of the square
          j, &          ! the location along the spiral
          shellnum, &   ! which spiral are we in
          cyc           ! position in that spiral

     do i = 0, n
        if (i == 0) then
           shellsize = 1
        else
           shellsize = i * 8
        end if
        if (n < shellsize) then
           shellnum = i
           cyc = n
           exit
        end if
        n = n - shellsize
     end do
     shellside = 2*shellnum + 1
     side = (cyc + 1 + shellside/2) / 4
     cyc = modulo(cyc, shellside)


     select case (side)
     case(1)
        x = n / 2 + shellside
        y = shellside - cyc + 1
     case(2)
        x = shellside - cyc + 1
        y = 1
     case(3)
        x = 1
        y = cyc
     case(4)
        x = cyc
        y = shellside
     end select


   end function



