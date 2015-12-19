
FC=gfortran
FCFLAGS=-g

% :: %.o
	$(FC) $(FCFLAGS) $< -o $@

%.o :: %.f90
	$(FC) $(FCFLAGS) -c $< 


recsplit: recmod.o recsplit.o 
	$(FC) $(FCFLAGS) recmod.o recsplit.o -o recsplit
clean:
	rm *.o *.mod
