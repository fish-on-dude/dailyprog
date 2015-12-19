
    subroutine irref(A, c, order, code)
        ! IRREF - reduced row echelon form via integer-only arithmatic
        !  Implements Gaussian elimination on matrix A
        !  C is a column vector solving Ac = 0
        !  ORDER tracks any row switches that occurred
        !  CODE is 0 if there is a unique solution, -1 if
        !   the system is infeasible, and 1 if more than one
        !   solution is possible
        use mathroutines   
        IMPLICIT NONE
 
                    integer, intent(inout) ::A(:,:)
                    integer, intent(out)   ::C(:)
                    integer, intent(out)   ::ORDER(:)
                    integer, intent(out)   ::CODE
 
        integer igcd, i, j, k, N, ifac1, ifac2
        integer nrows, ncols, lc, rowrank, colrank
 
        nrows=size(A, 1) !
        ncols=size(A, 2) !
 
        order = [(i,i=1,ncols)]
       
        N = minval([nrows,ncols])
 
        ! Decomposition (Elimination)
        DO k = 1, nrows
 
            !call prmat(A)
            if (all(A(k:,k:) == 0) ) exit
            if (all(A(k:,k ) == 0) ) then ! column is empty below pivot point
                do i=k+1,ncols
                    if (any(A(k:,i) > 0 )) then
                        call swap2(A(:,k), A(:,i))
                        call swap1(order(k), order(i))
                        exit
                    end if 
                end do
            end if
            if (    A( k, k) == 0 ) then
                ! look lower in current column for new pivot
                do i=k+1,nrows
                    if (A(i,k) > 0 ) then
                        call swap2(A(k,:), A(i,:))
                        exit
                    end if
                end do
            end if
 
            DO i = k+1, nrows
                if (A(i,k) == 0 ) cycle
                igcd = gcd([A(I,K) , A(K,K)])
                ifac1 = A(i,k)/igcd
                ifac2 = A(k,k)/igcd
                A(i,:) = ifac2*A(i,:) - ifac1 * A(k,:)
                A(i,:) = A(i,:) / gcd(A(i,:))  
            ENDDO
        ENDDO
        !call prmat(A)
   
        rowrank = count(sum(abs(A), 2) > 0)
        colrank = count(sum(abs(A), 1) > 0)
   
        select case(colrank - rowrank - 1)
        case(:-1)
            code = -1   ! Ac = 0 infeasible
        case(1:)
            code = 1    ! Ac = 0 indeterminate
        case(0)
            code = 0
        end select
   
        if (code < 0) return
   
        ! Back-substitution
        DO I = N, 2, -1
            if (A(i,i) ==0 ) cycle
            DO k= i-1, 1,-1
                if(A(k,i) == 0) cycle
                igcd = gcd([A(k,i) , A(i,i)])
                ifac1 = A(k,i)/igcd
                ifac2 = A(i,i)/igcd
                A(k,:)=ifac2*A(k,:)-ifac1*A(i,:)
                A(k,:) = A(k,:) / gcd(A(k,:))
            
            ENDDO
        ENDDO
 
        lc=lcm([(A(i,i), i=1,N)])
 
        do i=1,N
            if (A(i,i) == 0) cycle
            A(i,:) = A(i,:)*lc / A(i,i)
        end do
        if (code .ne. 0) return
   
        do i=1,ncols-1
            c(order(i)) = -A(i,ncols)
        end do
        c(order(Ncols)) = A(1,1)
        !call prmat(A)
   
        contains
        subroutine swap2(a,b)
            integer a(:), b(:), c(size(a))
            c = a
            a = b
            b = c
        end subroutine
        subroutine swap1(a,b)
            integer a, b, c
            c = a
            a = b
            b = c
        end subroutine
    end subroutine
 
