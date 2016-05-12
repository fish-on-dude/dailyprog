!!!   shannon.f90   !!!

    program shannon
    integer, parameter :: LLEN=180, MASCII=127
    character(len=LLEN) :: chars
    integer(1) :: ic(LLEN)
    integer :: cc(0:MASCII)
    real    ::f(0:MASCII)

    equivalence(chars, ic)
    
    READLINE: do 
        cc = 0
        read(10, '(A)', iostat=ios) chars
        if (ios .ne. 0) exit
        
        COUNTCHARS: do i=1,len_trim(chars)
            cc(ic(i)) = cc(ic(i)) + 1
        end do COUNTCHARS

        f = real(cc) / len_trim(chars)
        f = -1. *  f * log(f)
        
        entropy = sum(f, mask=cc.gt.0)/log(2.)
        
        print *, entropy

     end do READLINE
    end program shannon
  
