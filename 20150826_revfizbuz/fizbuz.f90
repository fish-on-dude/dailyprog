
      PROGRAM FIZBUZ
!     determine which numbers are represented by a, b, and c
!     input is standard input
!     a series of strings, a, b, c, ab, ac, bc, or abc
!     these represent increasing integers, which a, b, and c are factors
!     of.  If none of the numbers is a factor then the term is not
!     represented in the series.

      character*3 :: chline
      character*1 :: ch
      integer, parameter :: MX = 1, EQ = 2, MN = 3, AB=1, AC=2, BC=3
      
      dimension:: ratio(3, 3)
      dimension:: ncounter(3), ncurrent(3)
      ncounter = 0
      
      do
         read(5, *, end = 10) chline
         write(6, 1000) chline
         ncurrent = 0
         do i=1,3
            select case (chline(i:i))
            case('a')
               ichr = 1
            case('b')
               ichr = 2
            case('c')
               ichr = 3

            case default
               ichr = 0
            end select
            if (ichr .gt. 0) then
               ncurrent(ichr) = 1
               write(6, 1001) chline(i:i)
            end if
         end do
         do i=1,3
            if (ncurrent(i) .eq. 0) cycle
            do j=1,3
               if (i .eq. j) cycle
               if (ncurrent(j) .eq. 1) then
!     then we know the exact ratio betwen the two numbers
                  ratio(
         
      end do
 1000 FORMAT( 'I READ: ', A)
 1001 FORMAT( 'I SEE AN ', A)
 10   continue

      end program
