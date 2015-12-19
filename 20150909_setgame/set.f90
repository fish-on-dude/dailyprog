program set
implicit none
integer, parameter:: DIA=1,RED=4,ONE=7,OPE=10
integer key(12), i, j, k, icat, ichr, ibit, i_a, i_o, icase
integer(2) game(12)
character*(1) input(4,12)
logical lsphe, lcolo, lshad, lnmbr
key = iachar((/'D','O','S','R','P','G','1', '2', '3','O','H','F'/))
do icase = 1, 2
    print*, 'input set ', icase
    !do
    game = 0
    read(10,'(4a1)')(input(1:4,i), i=1,12)
    do concurrent (icat=1:4, ichr=1:3)
        ibit = (icat-1)*3+ichr
        where (iachar(input(icat, :)) == key(ibit) ) game = ibset(game, ibit)
    end do
    do concurrent(i=1:12, j=i+1:12, k=j+1:12)
        i_a = iall(game([i,j,k])) ! will be 000 if this category is not all the same
        i_o = iany(game([i,j,k])) ! will be 111 when this categorty is all different
        lsphe = ibits(i_a, DIA, 3) /=0  .or. ibits(i_o, DIA, 3) ==7
        lcolo = ibits(i_a, RED, 3) /=0  .or. ibits(i_o, RED, 3) ==7
        lshad = ibits(i_a, OPE, 3) /=0  .or. ibits(i_o, OPE, 3) ==7
        lnmbr = ibits(i_a, ONE, 3) /=0  .or. ibits(i_o, ONE, 3) ==7
        if (ALL([ lsphe, lcolo, lshad, lnmbr ]) ) then
            print'(3(4a1, 1x))', input(:,i), input(:, j), input(:, k)
        end if
    end do
end do
end program
