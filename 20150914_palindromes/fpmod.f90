
module pword_mod
  type pword
     character*(:), pointer :: word
     integer :: len
     integer :: longest_tail ! longest palindromic tailx
   contains
     generic, public::  operator(>) => greater_than
  end type pword

  interface pword
     procedure new_pword(word)
  end interface pword

contains
  function new_pword(word) result (npword)
    character*(:) :: word
    type(pword) npword
    integer length, j, lpe
    length = len(word)
    allocate(npword%word, source=word)
    npword%len = length
    do j=1, length
       if (word(length:length-j) .eq. word(length-j:length)) then
          npword%longest_tail = j
       end if
    end do
    
  end function new_pword
  
    
  
  logical function greater_than(a, b)
    class(pword) :: a, b

    if (a%len == b%len) then
       greater_than = a%word > b%word
    else
       greater_than = a%len > b%len
    end if
  end function greater_than
  
  
SUBROUTINE Bubble_Sort(a)
  type(pword), INTENT(in out), DIMENSION(:) :: a
  type(pword) :: temp
  INTEGER :: i, j
  LOGICAL :: swapped
 
  DO j = SIZE(a)-1, 1, -1
    swapped = .FALSE.
    DO i = 1, j
      IF (a(i) > a(i+1)) THEN
        temp = a(i)
        a(i) = a(i+1)
        a(i+1) = temp
        swapped = .TRUE.
      END IF
    END DO
    IF (.NOT. swapped) EXIT
  END DO
END SUBROUTINE Bubble_Sort
end module pword_mod

