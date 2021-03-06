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
 
    module chemeq_mod
        implicit none
        integer, parameter:: maxnelems = 350, maxnterms = 50
        integer:: nelems = 0, nterms = 0
        character*2 elemnames(maxnelems)
 
        contains
        subroutine parse_equation(aline, aterms, split, A)
            integer, allocatable, intent(out) :: A(:,:)
            integer :: temp(maxnelems, maxnterms)
            integer :: m(maxnelems)   
            integer split, i, iside, nt, iostat
       
            character, intent(in) :: aline*(*)
            character:: tterms*80(maxnterms)
            character, allocatable:: aterms*80(:)
            elemnames = ''
            nelems = 0
            nterms = 0
            tterms = ''
            read(aline, *, iostat=iostat) (tterms(i), i=1,size(tterms))
            iside = 1
            nt = 0
            do i=1,size(tterms)
                select case(tterms(i))
                case ('+')
                    cycle
                case ('->')
                    split = nt+1
                    iside = -1
                case default
                    if (tterms(i) == '') exit
                    nterms = nterms + 1
                    call proc_term(tterms(i), m)
                    temp(:, nterms) = m * iside
                end select
            end do
            if(allocated(aterms)) deallocate(aterms)
            allocate(aterms(i-1), source=tterms)
            allocate(A(nelems, nterms), source=temp)
 
        end subroutine
 
       integer function findelem(ename) result (n)
            character, intent(in):: ename*(*)
            integer i
   
            do i=1,nelems
                if (elemnames(i) == ename) then
                    n = i
                    return
                end if
            end do
            n = 0
        end function
 
        integer function addelem(ename) result (n)
            character, intent(in):: ename*(*)
            n = findelem(ename)
            if (n == 0) then
                nelems = nelems + 1
                elemnames(nelems) = ename
                n = nelems
            end if
        end function
 
        subroutine proc_term(aterm, mult_elems)
    
            ! given a chemical term, return a column vector of the multiplicities of the contained elements
            ! integer elemslots(maxnelems,2)
            ! index of location of first and last character in each element name in the term
            integer:: slots(maxnelems) , grouping(3, maxnelems) , & ! index of start and stop location and multiplicity for groups (including singles)
               slot_mult(maxnelems)
   
            character aterm*(*)
            integer:: mult_elems(:)
            integer :: m, ich , i, j
 
   
            integer :: slotnum , grouptab,maxgrouptab, ichnext, idxelem
            character :: en*2, digs*(10), lc*26
   
            slots = 0; grouping = 0; slot_mult = 0; mult_elems = 0; ich = 0;
            slotnum=0; grouptab = 0; maxgrouptab=0
            write(digs, '(10a1)') (i, i=iachar('0'), iachar('9'))
            write(lc,   '(26a1)') (i, i=iachar('a'), iachar('z'))
   
            do
                ich = ich + 1
                if (ich > len_trim(aterm)) exit
       
                select case(aterm(ich:ich))
                case('A':'Z')
                    slotnum = slotnum + 1
                    ichnext = verify(aterm(ich+1:), lc)
                    if (ichnext == 0) then
                        ichnext = ich
                    else
                        ichnext = ichnext -1 + ich
                    end if
 
                    read(aterm(ich:ichnext), *) en
                    idxelem = addelem(en)
                    slots(slotnum) = idxelem
                    ich = ichnext
           
                    grouptab = maxgrouptab + 1
                    grouping(1:2, grouptab) = slotnum
                    grouping(3, grouptab) = 1
                    maxgrouptab=grouptab
 
                case('0':'9')
                    ichnext = verify(aterm(ich+1:), digs)
                    if (ichnext == 0) then
                        ichnext = ich
                    else
                        ichnext = ichnext -1 + ich
                    end if
                    read(aterm(ich:ichnext), *) m
                    grouping(3, grouptab) = m
                    ich =  ichnext
           
                case('[','(')
                    grouptab = maxgrouptab + 1
                    grouping(1, grouptab) = slotnum +1
                    maxgrouptab=grouptab
           
                case(']',')')
                    do i=maxgrouptab, 1, -1
                         if(grouping(2, i) == 0) then
                            grouping(2, i) = slotnum
                            grouping(3, i) = 1
                            grouptab = i
                            exit
                        end if
                    end do              
                end select
            end do
            slot_mult = 1
            do i=1,maxgrouptab
                do j=grouping(1, i),grouping(2, i)
                    slot_mult(j) = slot_mult(j) * grouping(3, i)
                end do
            end do
            do i=1,slotnum
                mult_elems(slots(i)) = mult_elems(slots(i)) + slot_mult(i)
            end do
   
        end subroutine
 
        subroutine pr_eq(c, aterms, split)
            integer c(:)
            character aterms*(*)(:)
            character fmt*120
            integer split,i,nterms
            character subterms*(100)(size(c))
            nterms = size(c)
   
            do i=1,size(aterms)
                if(mod(i,2) ==0 .or. c(i/2+1) == 1) then
                    write(*,'(a,1x)',advance='no')trim(aterms(i))
                else
                    write(*,'(i0,a,1x)', advance='no')c(i/2+1), trim(aterms(i))
                end if
            end do  
            print*, ''
        end subroutine
 
    end module
 
   
    program chemeq
        use chemeq_mod
        use mathroutines
        IMPLICIT NONE
        include 'irref.fh'
        integer, allocatable :: A(:,:), c(:), order(:)
        character, allocatable :: aterms(:)
        integer ::  code, split
        character*256 aline
   
        do
            read(10, '(a)', end=10) aline
            if (allocated(A)) deallocate(A)
            call parse_equation(aline, aterms, split, A)
            if (allocated(c)) deallocate(c)
            if (allocated(order)) deallocate(order)
            allocate(c(nterms), order(nterms))
            call irref(A, c, order, code)
            select case(code)
            case (-1)
                print*, aline, 'infeasible:'
                call prmat(A)
            case (1)
                print*, aline
                print*, 'multiple solutions:'
                call prmat(A)
            case default
                call pr_eq(c, aterms, split)
            end select
        end do
           
        10 continue
    end program
                                
    
    
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
 
