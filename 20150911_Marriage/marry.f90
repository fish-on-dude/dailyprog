module reviewer_m
implicit none
type:: reviewer
     character*1,allocatable :: prefs(:)
     character*1 :: name
     class(reviewer), pointer :: partner
     logical :: free
     contains
     procedure, pass :: prefer
     procedure, pass :: engage
end type reviewer

type, extends (reviewer) :: suitor
   logical, allocatable :: proposed(:)
 contains
   procedure, pass :: choose_next
end type suitor
contains
    logical function prefer(self, suitor)
    class(reviewer) self
    character*1 suitor
    integer i
    prefer = .TRUE.
    if (self%free) return
    do i=1,len(self%prefs)
       if (self%prefs(i)== suitor )then
          return
       elseif(self%partner%name == suitor)then
          prefer = .FALSE.
          return
       end if
    end do
  end function prefer
  
  subroutine engage(self, suitor)
    class(reviewer) self
    class(*), pointer :: suitor
    select type(suitor)
       class default
    if (associated(self%partner)) then
       self%partner%free = .TRUE.
    end if
    select type(suitor)
       class is(reviewer)
          self%partner => suitor
       class is(suitor)
          self%partner => suitor
          
    end select
  end subroutine engage

  function choose_next(self, options) result(choice)
    class(suitor) self
    type(reviewer), target :: options(:)
    type(reviewer), pointer :: choice
    integer i,n
    do i=1,len(self%prefs)
       if(self%proposed(i))cycle
       n = iachar( self%prefs(i))-iachar('a')+1
       choice => options(i)
       self%proposed(i) = .TRUE.
       exit
    end do
  end function choose_next
end module reviewer_m


program marry
  ! implement Gale-Shapley algo
  use reviewer_m
  implicit none
type (reviewer), allocatable, target:: reviewers(:)
class (suitor), allocatable, target:: suitors(:)
integer i, j, k , case! loop vars
class (reviewer), pointer:: try


do case =1,2
   call read_case
   ANYFREE: do 
      do j=1, size(suitors)
         if(.not.suitors(j)%free) cycle
         try => suitors(j)%choose_next(reviewers)
         if(try%prefer(suitors(j)%name))then
            call try%engage(suitors(j))
            call suitors(j)%engage(try)
            cycle ANYFREE
         end if
      end do
      !only get here if none are free
      exit ANYFREE         
   end do ANYFREE
end do
contains
  
    
    
subroutine read_case
 integer N
 character*(256) :: aline
 character*(1), allocatable :: chars(:)
 read(10, *), N
 print*, N
 allocate(chars(N+1))

 ! allocate arrays
 if (case > 1) deallocate(suitors, reviewers)
 allocate(suitors(N), reviewers(N))
 ! allocate(chars(N))
 do i=1,N
    allocate(suitors(i)%prefs(N), suitors(i)%proposed(N))
    allocate(reviewers(i)%prefs(N))
 end do
 ! read suitor info
 do i=1,N
    read(10, '(a)') aline
    read(aline, *) chars
    suitors(i)%name = chars(1)
    suitors(i)%prefs= chars(2:)
 end do
 ! read reviewer info
 do i=1,N
    read(10, '(a)') aline
    read(aline, *) chars
    reviewers(i)%name=chars(1)
    reviewers(i)%prefs=chars(2:)
 end do

 ! read out suitors
  do i = 1,N
     print*, suitors(i)%name
     print*, (suitors(i)%prefs(j), ',', j=1,N)
  end do

end subroutine read_case

end program



