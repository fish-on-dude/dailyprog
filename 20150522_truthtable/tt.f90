program tt

real f(10)
character*(10) chdisp, ch(10)
integer n
logical b

80 format(5a10)
   print 80,  'type', 'value', 'i/o', 'transfer', 'if(n)'
   print 80, ('---------', i=1,5)

90 format(a10, i10, 3l10)

   do n=-2,3
    b = .false.
    if(n) b = .true.
    print 90, 'integer',n, n, transfer(n, .true.), b
   end do

92  format(a10, g10.2, 2l10, '  incompat') 

    f (1:5)=(/0.0, -1.23, 1.0, huge(0.0),tiny(0.0)/)    
    do n=1,5
       ! if(f(n)) b = .true.  (compiler error on ifort)
        print 92, 'real', f(n), f(n),transfer(f(n), .true.)
    end do



94      format(a10, a10, a10, l10, a10)       
    !if('foo') b = .true.  (compiler error on ifort)
    print 94, 'character', 'char(1)', 'incompat', transfer(char(1), .true.), 'incompat'
    print 94, 'character', '''a''', 'incompat', transfer('a', .true.), 'incompat'                
    print 94, 'character', '''b''', 'incompat', transfer('b', .true.), 'incompat'
    print 94, 'character', '''abb''', 'incompat',  transfer('aab', .true.), 'incompat'
    print 94, 'character', '''baa''', 'incompat',  transfer('baa', .true.), 'incompat'
    print 94, 'character', ''' ''', 'incompat', transfer(' ', .true.), 'incompat'
    print 94, 'character', '''''',  'incompat', transfer('', .true.), 'incompat'

93  format(a10, 2f5.1,2( l5, ',', l4))
    print 93, 'array', (/1.0, 0.0/), (/1.0, 0.0/), &
               transfer((/1.0,0.0/), .true., 2)

95 format(a10, 2f5.1, a10, l5, ',',l4)  
    print 95, 'complex', (0.0, -1.0),  'incompat' , &
              transfer((0.0,-1.0), .true., 2)

91  format(a10, a10, 2l10)
    print 91, 'logical', '.true.', .true., transfer(.true., .true.)
    print 91, 'logical', '.false.', .false., transfer(.false., .true.)



end program
