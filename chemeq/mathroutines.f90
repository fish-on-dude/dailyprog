module mathroutines
        contains
 
        recursive integer function lcm(n) result(g)
            integer, intent(in) :: n(:)
            integer :: isize
            integer u, v
            isize = size(n)
            if (isize > 2) then
                u = n(1)
                v = lcm(n(2:))
            else if (isize == 2) then
                u = n(1)
                v = n(2)
            end if
            if (u == 0) u = 1
            if (v == 0) v = 1
            g = abs(u * v) / gcd([u,v])
        end function lcm
 
        recursive integer function gcd(n) result(g)
            integer, intent(in) :: n(:)
            integer :: isize
            integer u, v
            isize = size(n)
            if (isize > 2) then
                u = n(1)
                v = gcd(n(2:))
            else if (isize == 2) then
                u = n(1)
                v = n(2)
            else
                g = 1
                return
            end if
            if (all([u,v]==0)) then
                g = 1
            else if (v == 0) then
                g = u
            else if (mod(u, v) /= 0) then
                g = gcd([v, mod(u, v)])
            else
                g = v
            end if
            g = abs(g)
        end function gcd
 
        subroutine prmat(A)
            integer:: A(:,:)
            do i=1,size(A,1)
                write(*,'(*(i5))') (A(i,j),j=1,size(A,2))
            end do
            print*, '========='
        end subroutine
 
    end module
