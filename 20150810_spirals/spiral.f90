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

   !       5-4-3
   !       6 1-2
   !       7-8-9-10
   !

   !
   ! say each corner is the "end" of the previous side.  The next
   ! side starts after turning the corner.
   ! So for shell number 2 above, the following numbers are in each
   ! side:
   !
   !  Side       Cells
   !   1         2, 3
   !   2         4, 5
   !   3         6, 7
   !   4         8, 9
   !
   ! Cell number 10 is the first cell of shell 3.
   ! 
   
   subroutine spiral_to_location(n, spiralpos, x, y)
     !  a n=(2s+1) square has s shells
     !  s = (n-1)/2
     !  shell 1 is the center cell
     !  shell 2 has 8 cells
     !  shell 3 has 16 cells
     !  shell n has 8*(n-1) cells

     intent(in) :: n, spiralpos
     intent(out):: x, y
     integer(8) ::      &  
          spiralpos,    & ! the location along the spiral
          shellnum,     & ! which spiral are we in
          cyc             ! position within the spiral
     integer    ::      &
          n,            & ! the size of the squares
          i,            & ! loop var
          s,            & ! number of shells
          side,         & ! side 1-4
          x, y            ! x and y position in the grid. Row-major.
                          ! top-left corner = 1,1; t-r = n, 1

     s = (n-1)/2 !shell number
     SPIRAL: do i = 1, s
        if (i == 1) then
           shellsize = 1
        else
           shellsize = (i-1) * 8
        end if
        if (n < shellsize) then
           shellnum = i
           cyc = n
           exit
        end if
        n = n - shellsize
     end do SPIRAL
     side = cyc / s  
     cyc = modulo(cyc, s)


     select case (side)
     case(1)
        x = s - 
        y = 
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



