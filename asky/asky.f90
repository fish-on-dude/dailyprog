    program askymod
      implicit none
      integer, allocatable :: building(:, :)
      character, allocatable:: blueprint(:,:);
      integer, parameter:: BP_MAXWIDTH=32, &
           SCALE_W = 4, SCALE_H = 3, &
           INT_BIT =1, WALL_BIT=2,&
           TRUSS_BIT = 3, ROOF_BIT=4, WINDOW_BIT=5, DOOR_BIT=6,&
           DOORJAM_BIT = 7, RSH_BIT=8, LSH_BIT =9, FLOOR_BIT = 10
      integer :: BP_WIDTH, BLDG_WIDTH, BP_HEIGHT, BLDG_HEIGHT
    
    
      integer N, i, j, idoor
      integer, allocatable, dimension(:,:):: L, R, LB, B, RB, T
      logical, allocatable ::TMP(:)
      character*(BP_MAXWIDTH) aline
      integer idx
      read(10, *)  BP_HEIGHT 
      allocate(blueprint(0:BP_MAXWIDTH, 0:BP_HEIGHT))
      do i=BP_HEIGHT, 1, -1
         !print*, i
         read(10, '(A)') aline
         !print*, aline
         idx = len_trim(aline)+1
         blueprint(1:idx, i) = transfer(aline(:idx), blueprint, idx)
    
         print*, blueprint(:, i)
      end do
      do i=1,BP_MAXWIDTH
         print*, i, blueprint(i, 1)
         if (blueprint(i, 1) == ' ') then
            BP_WIDTH = i
            exit
         end if
      end do
      print*, BP_WIDTH
    
      BLDG_WIDTH = BLW(BP_WIDTH+1)
      BLDG_HEIGHT = BLH(BP_HEIGHT+1) + BLDG_WIDTH/2
      print*, BLDG_WIDTH, BLDG_HEIGHT
      allocate(building(BLDG_WIDTH, BLDG_HEIGHT))
      building = 0
      building(:,1) = ibset(building(:, 1), FLOOR_BIT)
      do concurrent( i=1: BLDG_WIDTH, j=1: BLDG_HEIGHT)
         if(BP(i,j)) &
              call setbit(building(i,j), INT_BIT)
      end do
      do concurrent(i=1:BP_WIDTH, j=1:BP_HEIGHT)
         call setbit(building(BLW(i), BLH(j)), WINDOW_BIT)
      end do
    
      ! allocate(L, R, LB, B, RB, T,TMP, MOLD=building)
    
      !  R = eoshift(building, -1)
      !  T =  eoshift(building, -1,dim=2)
      allocate(tmp(BLDG_WIDTH))
    
      where (btest(iand(building, not(eoshift(building,-1))),&
           INT_BIT))
         building = ibset(building, WALL_BIT)
      end where
      where (btest(iand(eoshift(building,-1), not(eoshift(building,0))),&
           INT_BIT))
         building = ibset(building, WALL_BIT)
      end where
    
    
      where (btest(IAND(building,  NOT(eoshift(building, 1,dim=2))), INT_BIT)) 
         building = ibset(building, TRUSS_BIT)
      end where
    
      where(btest(eoshift(building, 1, dim=2), WALL_BIT) .and.&
           btest(IOR(eoshift(building, 1), eoshift(building, -1))&
           , TRUSS_BIT))
         building = ibset(building, TRUSS_BIT)
         building = ibset(building, WALL_BIT)
      end where
    
    
      where(btest(eoshift(building, 0), WALL_BIT) .and.&
           btest(IOR(eoshift(building, 1), eoshift(building, -1))&
           , TRUSS_BIT))
         building = ibset(building, TRUSS_BIT)
      end where
    
      where(btest(IEOR(building, IAND(eoshift(building, 1), &
           eoshift(building, -1))), TRUSS_BIT))
         building = ibset(building, WALL_BIT)
      end where
    
    
    
      where(btest(building, TRUSS_BIT))
         building = ibset(building, ROOF_BIT)
      end where
    
      do i=1, BLDG_HEIGHT
         tmp = btest(building(:,i-1), ROOF_BIT)
         where (tmp.and.eoshift(tmp, -1).and.eoshift(tmp, 1))&
              building(:,i) = ibset(building(:, i), ROOF_BIT)
    
      end do
    
      where (btest(iand(building, not(eoshift(building, -1))), ROOF_BIT))&
           building = ibset(building, RSH_BIT)
    
      where (btest(iand(building, not(eoshift(building, 1))), ROOF_BIT))&
           building =ibset(building, LSH_BIT)
    
    
      do concurrent(i=1:BP_WIDTH, j=1:BP_HEIGHT)
         call setbit(building(BLW(i), BLH(j)), WINDOW_BIT)
      end do
    
      idoor = 1+ BP_WIDTH * rand(1)
      print*, 'door' , idoor
      call setbit(building(BLW(idoor)-1, 2), DOORJAM_BIT)
      call setbit(building(BLH(idoor)+1, 2), DOORJAM_BIT)
    
      do i=BLDG_HEIGHT, 1, -1
         !write(*, '(i1)', advance='no') i
         do j=1,BLDG_WIDTH
            write(*, '(A1)', advance='no')c(building(j,i))
         end do
         print*,''!']'
      end do
    
    
    contains
      elemental integer function BLH(i)
        integer, intent(in) :: i
        BLH = (i-1)*SCALE_H+1
      end function BLH
      elemental integer function BLW(i)
        integer, intent(in) :: i 
        BLW = (i-1)*SCALE_W+1
      end function BLW
      elemental integer function BPH(i)
        integer, intent(in) :: i
        BPH = (i-1)/SCALE_H+1
      end function BPH
      elemental integer function BPW(i)
        integer, intent(in) :: i
        BPW = (i-1)/SCALE_W+1
      end function BPW
      elemental logical function BP(x,y)
        integer, intent(in):: x,y
        BP = blueprint(BPW(x), BPH(y)) == '*'
      end function BP
      elemental subroutine setbit(int, bit)
        integer, intent(inout) :: int
        integer, intent(in):: bit
        int = ibset(int, bit)
      end subroutine setbit
      elemental logical function q(int, bit)
        integer, intent(in) :: int, bit
        q = btest(int, bit)
      end function q
      character function c(int)
        integer, intent(in) :: int
        if(q(int, WALL_BIT).and.q(int, TRUSS_BIT)) then
           c = '+'
        else if (q(int, WALL_BIT)) then
           c = '|'
        else if (q(int, TRUSS_BIT)) then
           c = '-'
        else if (q(int, FLOOR_BIT)) then
           c = '_'
        else if (q(int, RSH_BIT).and.q(int, LSH_BIT)) then
           c = 'A'
        else if (q(int, LSH_BIT)) then
           c = '\\'
        else if (q(int, RSH_BIT)) then
           c = '/'
        else if (q(int, DOORJAM_BIT)) then
           c = '|'
        else 
           c = ' '
        end if
      end function c
    
    
    end program askymod
    
