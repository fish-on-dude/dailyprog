program gs
  implicit none
  integer, allocatable::sprefs(:,:),rprefs(:,:),&
       rmatch(:),smatch(:)
  logical, allocatable:: isfree(:,:),proposed(:,:)
  integer N, icase, i,j,k, jrev,ksuit
  character*1,allocatable :: chars(:,:,:)
  do icase=1,2
     read(10,*) N
     if (icase>1) deallocate(sprefs, rprefs,&
          rmatch,smatch, isfree, proposed, chars)
     allocate(sprefs(N,N), rprefs(N,N), rmatch(N),&
          smatch(N),isfree(N,2),proposed(N,N),&
          chars(N+1,N,2))
     read (10,*) chars
     sprefs =iachar(chars(2:,:,1))-iachar('a')+1
     rprefs =iachar(chars(2:,:,2))-iachar('A')+1
     proposed=.false.
     isfree=.true.
     rmatch=0
     anyfree: do
        scansuit: do i=1,N
           if(.not.isfree(i,1))cycle scansuit! suitor engaged
           scanrev: do jrev=1,N
              j=sprefs(jrev,i)
              if(proposed(j,i)) then
                 !print*,'already proposed to', j, 'skipping'
                 cycle scanrev! try next reviewer
              end if
              !print*,'proposing to', j
              proposed(j,i)=.true.
              checkpref: do ksuit=1,N
                 k=rprefs(ksuit,j)
                 if (k==i) then
                    if(rmatch(j)>0) then
                       !print*, j, 'was engaged to', rmatch(j)
                       !print*, rmatch(j) ,'is now free'
                       isfree(rmatch(j),1)=.true.
                    end if
                    rmatch(j)=i
                    !print*,j, 'and', i, ' are now engaged'
                    isfree( j,2) =.false.
                    isfree( i,1) =.false.
                    cycle anyfree ! go back to start of list
                 else if(k==rmatch(j)) then
                    cycle scanrev! rejected, try next reviewer
                 end if
              end do checkpref
           end do scanrev
           cycle anyfree
        end do scansuit
        ! only get here if all suitors are taken
        exit anyfree
     end do anyfree
     do k=1,N
        smatch(rmatch(k))=k
     end do
     do i=1,N
100     format('(', a1, ',' , a1, ')')
        print 100, char(i+iachar('A')-1),&
             char(smatch(i)+iachar('a')-1)
     end do
  end do
end program gs
