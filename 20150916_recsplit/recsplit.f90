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
        
    
    
    
    
    
    
    
    
