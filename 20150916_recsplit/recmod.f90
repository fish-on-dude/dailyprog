    module rec_splitline_mod
      implicit none
      type subdivision
         logical:: is_split = .false.
         type(subdivision) , pointer :: part1, part2
         integer split_dim ! 1 = split parallel to cols, 2 = parallel to rows
         integer split_loc ! location of the horizontal or vertical split 
       contains
         procedure, pass :: split => split_subdivision
         procedure, pass :: print => print_subdivision
      end type subdivision
    contains
      
      subroutine print_array(array)
        integer array(:,:), arrydim(2), i, j
        
        arrydim = shape(array)
           do i=1,arrydim(2)
             do j=1,arrydim(1)
                write(*, '(i2, x)', advance='no') array(j, i)
             end do
             print*, ''
         end do
       end subroutine print_array
       
      recursive subroutine split_subdivision(subdiv,array,N)
        class(subdivision) :: subdiv
        integer array(:,:)
        integer, allocatable:: sa1(:,:), sa2(:,:)
        integer N, A, B, bestdim, bestsplit, arrydim(2), i, j
        real thisscore, bestscore
        if (N == 1) then
           print*, arrysum(array)
           return
        end if
        subdiv%is_split = .true.
        A = FLOOR(N/2.)
        B = CEILING(N/2.)
        arrydim = shape(array)
        bestscore = huge(bestscore)
        do i = 1,2
           do j=1,size(array, i)-1
              thisscore = score(i, j, A, B)
              if (thisscore < bestscore) then
                 bestdim = i
                 bestsplit = j
                 bestscore = thisscore
              end if
           end do
        end do

        subdiv%split_loc = bestsplit
        subdiv%split_dim = bestdim

        if (bestdim == 1) then 
           allocate(sa1(bestsplit,            arrydim(2)), &
                    sa2(arrydim(1)-bestsplit, arrydim(2)))
           sa1 = array(:bestsplit, :)
           sa2 = array( bestsplit+1:, :)
        else
           allocate(sa1(arrydim(1), bestsplit), &
                    sa2(arrydim(1), arrydim(2)-bestsplit))
           sa1 = array(:,:bestsplit)
           sa2 = array(:,bestsplit+1:)
        end if
        if (arrysum(sa1) < arrysum(sa2) .neqv. A<B) call swap(A,B)
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
          real  score
          integer A, B, split_index, pop1, pop2, N, itmp, ndim, i
          integer, allocatable :: sumvector(:)
          real lab, l12, lr
          character adum
          lr(i) = log(real(i))
          N = size(array, ndim)
          allocate(sumvector(N))
          sumvector = sum(array, 3-ndim)
          pop1 = sum(sumvector(:split_index))
          pop2 = sum(sumvector(split_index+1:))
          score = abs(abs(lr(A)-lr(B))-abs(lr(pop1)-lr(pop2)))
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
