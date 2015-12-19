program vn

  implicit none
  real start_time, end_time
  integer(8), allocatable :: ifact(:) 
  logical okay
  integer(8) :: i, k, p, minf, maxf
  integer :: n , m , j, dc(0:9),  nfangsfound, iostat
  do
     read(10,*, iostat=iostat) n, m ! # of digits in the number, # of fangs
     if (iostat /=0) exit

     ! print*, n,m
     nfangsfound = 0
     if(allocated(ifact))deallocate(ifact)
     allocate(ifact(m))
     if (mod(n,m) /=0) stop
     call cpu_time(start_time)

     minf = 10**(n/m-1)
     maxf = 10*minf-1
     !ifact = minf
20   format(i12.6, '=', *(i6.2:'*'))
     print*, 'looping from ', 10_8**(n-1), 'to', 10_8**(n)-1
     !$OMP PARALLEL DO DEFAULT(NONE) &
     !$OMP  PRIVATE(k,j,dc,ifact,p) &
     !$OMP  SHARED(n,m) REDUCTION(+:nfangsfound) 
     do i=10_8**(n-1),10_8**(n)-1!  while ( iterate(ifact, minf, maxf))
        k=i
       ! print*, 'n m k', n,m,k
        do j=1,m
           ifact(j)=modulo(k,10**(n/m))
           k = k/(10**(n/m))
        end do

        if (any(ifact(2:)<ifact(:m-1)))cycle
        if (any(ifact < 10_8**(n/m-1))) cycle
        if (count(mod(ifact,10)==0)>1) cycle ! more than one trailing zero
        if (sum(log10(real(ifact)))<n-1) cycle 

        dc = 0
        do j=1,m
           dc = dc + digitcount(ifact(j))
        end do

        p = product(ifact,1)
        !write(*,'(i11,2i6,10i2,i11)') i, ifact, dc,p
        if (all(dc == digitcount(p))) then
           nfangsfound = nfangsfound + 1
           write(11,*) p, ifact
        end if
     end do
     !$OMP END PARALLEL DO


     call cpu_time(end_time)
     write(*,'(2i4,i10,a20)')n,m,nfangsfound, fmttime(end_time-start_time)

  end do

contains
  pure function digitcount(num) result(dc)

    integer dc(0:9)
    integer(8), intent(in) :: num
    integer(8)::  n
    integer(8):: d

    n = num
    dc = 0
    do while (n > 0)
       d = mod(n, 10_8)
       dc(d) = dc(d) + 1
       n = n / 10
    end do

  end function digitcount

  recursive function iterate(ifac,imin, imax) result(okay)
    logical okay
    integer ifac(:)
    integer n, imin, imax

    n = size(ifac)
    if (ifac(n) < imax) then
       ifac(n) = ifac(n) + 1
       okay = .true.

    else if (n == 1) then
       okay = .false.
    else 
       okay = iterate(ifac(1:n-1), imin, imax)
       ifac(n) = ifac(n-1)
    end if
  end function iterate
  function fmttime(t)
    character(20) fmttime
    real t
    integer i, it, time(3), n
    integer, parameter:: ifac(3) = [1000, 60, 60]

    it = int(t * 1000)

    do i=1,3
       time(i) = mod(it, ifac(i))
       it = it/ifac(i)
       if (it>0) n = i+1
    end do
30  format (t16,".", i0.3,:, TL8, i4.0:, TL8, i4.0, "m")
    write(fmttime,30) time(:n)
  end function fmttime


end program vn
