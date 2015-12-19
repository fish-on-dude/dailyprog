
FC=gfortran
FCFLAGS=-g

% :: %.f90
	$(FC) $(FCFLAGS) $< -o $@

findpal : fpmod.mod findpal.f90
	%(FC) $(FCFLAGS) $< -o $@
