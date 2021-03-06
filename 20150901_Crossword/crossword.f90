program crossword
  implicit none
  character*1, allocatable:: grid
  character*(:), allocatable :: words(:)
  character*(256):: aline
  integer i
  
  read(10,'(a)')aline
  read(aline, '(5a)') words

  contains
  

subroutine addword( grid, word, score)
  character*1, intent ( inout):: grid(:,:)
  character*1, intent ( in):: word(:)
  integer score
  integer x,y, xdir, ydir, wordlen, best(4), &
       ysize, xsize, yend, xend, iscore
  real  maxscore
  logical fits
  xsize =size (grid,1)
  ysize =size(grid,2)
  do concurrent (x=1:xsize, y=1:ysize, xdir=-1:1, ydir=-1:1, &
       xdir*ydir==0 .AND. xdir+ydir/=0 )
     xend= x+wordlen*xdir
     if (xend<1.or.yend>ysize) cycle
     yend= y+wordlen*ydir
     if (yend<1.or.yend>ysize) cycle
     score=ctrscore(x,y,xend,yend)
     iscore = comboscore(grid,word,x,y,xdir,ydir, fits)
     if(.not.fits)cycle
     if (score+iscore>maxscore) then
        maxscore=score + iscore
        best=[x,y,xdir,ydir]
     end if
  end do
end subroutine addword

  pure function ctrscore(xsize, ysize,xstart,ystart,xend, yend)result(score)
    ! position score... penalize for manhattan distance from center of grid
    integer, intent(in) :: xsize, ysize, xstart, ystart, xend, yend
    integer i,x,y
    real xctr, yctr, dist, score
    xctr = (x + xend)/2.
    yctr = (y + yend)/2.
    score =1. - abs( xsize / 2. - xctr)/xsize -&
         abs( ysize/2. - yctr)/ysize
  end function ctrscore

  pure function comboscore(grid,word,xstart,ystart,xdir,ydir) &
       result(iscore)
    ! return value is numer of overlapping positions.  if the word
    ! does not fit in the given position, return -1
    character*1, intent(in) :: grid(:,:), word(:)
    integer iscore
    integer, intent(in):: xstart,ystart,xdir,ydir
    integer x,y,n, wl


    wl=size(word)
    do n=1, len(word)
       x=xstart+n*xdir
       y=ystart+n*ydir
       if (grid(x,y) == ' ') cycle
       if (grid(x,y) /= word(n)) then

       else
          iscore =iscore +1
       end if
    end do
  end function comboscore

end program crossword
