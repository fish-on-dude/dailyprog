program findpalindrome
  integer, parameter :: NUMWORDS = 172820, WORDSIZE = 50

  type(pword) :: dictionary(NUMWORDS)

  character*(WORDSIZE) :: aline
  
  integer numlines

  open(10, file='enable1.txt')
  do i=1,NUMWORDS
     read(10, '(a)') aline
     dictionary(i)%len = len_trim(aline)
     dictionary(i)%word = trim(aline)
  end do


  
end program findpalindrome
