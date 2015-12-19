program demobit
integer, parameter :: nbytes = 8, nbits = nbytes * 8

character*(nbits) ::  buffer
integer n, k
integer(nbytes) a

read(10, '(b<nbits>)') a
call printbuffer
do n = 1, nbits/2 -1
  a = xor(ishft(a,1), ishft(a,-1))
  call printbuffer
end do
contains

subroutine printbuffer
  integer n
    write(buffer, '(bz, b<nbits>)') a
    do n=1,nbits
        select case(buffer(n:n))
        case('0')
            buffer(n:n) = ' '
        case('1')   
            buffer(n:n) = 'X'
        end select
    end do
    write(*, '(a)') buffer
    end subroutine
end program
