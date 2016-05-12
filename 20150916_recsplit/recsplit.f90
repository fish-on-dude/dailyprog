    module rec_splitline_mod
      implicit none
      type subdivision
         integer census ! number of residents
         logical:: is_split = .false.
         type(subdivision) , pointer :: part1, part2
         integer split_dim ! 1 = split parallel to cols, 2 = parallel to rows
         integer split_loc ! location of the horizontal or vertical split 
       contains
         procedure, pass :: split => split_subdivision
         procedure, pass :: print => print_subdivision
      end type subdivision
    contains
    
      recursive subroutine split_subdivision(subdiv,array,N)
        class(subdivision) :: subdiv
        integer array(:,:)
        integer, allocatable:: sa1(:,:), sa2(:,:)
        integer N, A, B, shortest, longdim, arrydim(2), i, j
        real thisscore, bestscore
       ! print*, shape(array), N
        arrydim = shape(array)
          ! do i=1,arrydim(2)
          !   do j=1,arrydim(1)
          !      write(*, '(i2, x)', advance='no') array(j, i)
          !   end do
          !   print*, ''
         !end do
    
        if (N == 1) then
           print*, arrysum(array)
           return
        end if
        subdiv%is_split = .true.
        A = FLOOR(N/2.)
        B = CEILING(N/2.)
    
    
        subdiv%census = arrysum(array)
        bestscore = huge(bestscore)
        if(arrydim(1)<arrydim(2))then
           longdim = 2
        else
           longdim = 1
        end if
        
           do j=1,arrydim(longdim)-1
              thisscore = score(longdim, j, A, B)
              if (thisscore < bestscore) then
                 !print*, i, j
                 shortest = j
                 bestscore = thisscore
              end if
           end do

    
        subdiv%split_loc = shortest
        subdiv%split_dim = longdim
        !print*, sum(array, shortdim)
        if (longdim == 1) then 
           allocate(sa1(shortest,            arrydim(2)), &
                    sa2(arrydim(1)-shortest, arrydim(2)))
    

          ! print*, 'sa1', shape(sa1)
          ! print*, 'sa2', shape(sa2)
          ! print*, 'horizontal split at ', shortest

           sa1 = array(:shortest, :)
           sa2 = array( shortest+1:, :)
    
        else
           allocate(sa1(arrydim(1), shortest), &
                    sa2(arrydim(1), arrydim(2)-shortest))

           !print*, 'sa1', shape(sa1)
           !print*, 'sa2', shape(sa2)
           !print*, 'vertical split at ', shortest
           sa1 = array(:,:shortest)
           sa2 = array(:,shortest+1:)
        end if
        if (arrysum(sa1) < arrysum(sa2) .neqv. A<B) call swap(A,B)
        !print*, 'A', A, 'B', B
        !print*, 'arrysum', arrysum(sa1), arrysum(sa2)
    
        allocate(subdiv%part1, subdiv%part2)
        call subdiv%part1%split(sa1, A)
        call subdiv%part2%split(sa2, B)
      contains
        integer function arrysum(A)
          integer A(:,:)
          arrysum = sum(sum(A,1),1)
        end function arrysum
        subroutine swap(A, B)
          integer a, b, itmp
          itmp = A
          A = B
          B = itmp
        end subroutine swap
        function score(ndim, split_index, A, B)
          real ratio, score
          integer A, B, split_index, pop1, pop2, N, itmp, ndim
          integer, allocatable :: sumvector(:)
          real lab, l12
          N = size(array, ndim)
          !print*, 'N' , N
          allocate(sumvector(N))
          !print*, array
          sumvector = sum(array, 3-ndim)
          !print*, 'sumvector size', shape(sumvector)
          
          pop1 = sum(sumvector(:split_index))
          pop2 = sum(sumvector(split_index+1:))
          if (pop1<pop2 .neqv. A<B) call swap(A,B)
    
          if (B.EQ.0.OR.pop2.EQ.0.or.A.EQ.0.or.pop1.EQ.0) then
             score = huge(score)
          else
             score = abs(real(A)*real(pop2)- real(B)*real(pop1))
             
             ! score = abs(real(A)/real(B) -real(pop1)/real(pop2))
             !score = abs(lab-l12)
          end if
          !write(*,'(A, 2I2, 2I4, F8.3)'),'A B P1 P2 SCORE', A, B, pop1, pop2, score
        end function score
      end subroutine split_subdivision
      recursive function print_subdivision(subdiv, array) result (outarray)
        class(subdivision) ::subdiv
        character :: array(:,:)
        character  :: outarray(size(array, 1), size(array,2))
        integer ndim(2)
    
        outarray = array
        if (.not.subdiv%is_split) return
        ndim = shape(array)
        if(subdiv%split_dim == 1) then
           outarray(subdiv%split_loc*2, :) = '|'
           outarray(:subdiv%split_loc*2-1, :) =&
                  subdiv%part1%print(array(:subdiv%split_loc*2-1, :))
           outarray(subdiv%split_loc*2+1:, :) = &
                subdiv%part2%print(array(subdiv%split_loc*2+1:,:))
        else
           outarray(:, subdiv%split_loc*2) = '-'
           outarray(:, :subdiv%split_loc*2-1) = &
                subdiv%part1%print(array(:, :subdiv%split_loc*2-1))
           outarray(:, subdiv%split_loc*2+1:) = &
                subdiv%part2%print(array(:, subdiv%split_loc*2+1:))
        end if
      end function print_subdivision
      
        
    end module rec_splitline_mod
    program recsplit
      use rec_splitline_mod
      implicit none
      integer N, ix, iy, arrydim(2), i, j
      integer, allocatable :: country(:,:)
      character*1, allocatable:: printmat(:,:)
      logical, allocatable :: printmask(:,:)
      type(subdivision) sd
      read(10,*)N, ix, iy
      allocate(country(ix,iy))
      allocate(printmat(ix*2-1, iy*2-1))
      allocate(printmask(ix*2+1, iy*2+1))
     
      read(10, *) country
      printmat = ' '
      printmask = .false.
      do i=1,ix
         do j=1,iy
            write(printmat(2*i-1,2*j-1), '(i1)') country(i,j)
         end do
      end do
    
    
      !print*, 'country(2, 1) should be 5: ', country(2,1)
      call sd%split(country, N+1)
    
      printmat = sd%print(printmat)
      
      arrydim = shape(printmat)
      do i=1,arrydim(1)
         write(*, '(4x)', advance='no')
         do j=1,arrydim(2)
            write(*, '(a1)', advance='no') printmat(j, i)
         end do
         print*, ''
      end do
    
    end program recsplit
        
    
    
    
    
    
    
    
    
