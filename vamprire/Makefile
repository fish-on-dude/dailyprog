
FC=gfortran
FCFLAGS=-O3 -fopenmp
LIBS = c:/mingw/lib/

% :: %.f90
	$(FC) $(FCFLAGS) -L$(LIBS)  $< -o $@
