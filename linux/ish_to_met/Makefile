VPATH = ../../source/

# Definition of object variables

obj_serial = ish_to_met.o 

#*** General Purpose Rules ***
no_target:
	@echo \******** You did not specify a make target \********
	@echo Please read the comments at the top of the makefile

setup:
%.o : %.mod

.SUFFIXES: .f90 .for .o

.f90.o .for.o:
	$(FCOMPL) -c $(FFLAGS) $<

# Intel Compiler:
intel_64 : FFLAGS = -m64 -axCORE-AVX2 -O3 -ipo -no-prec-div -fpp -assume byterecl
intel_64 : LFLAGS = -static-intel
intel_64 : FCOMPL = ifort
intel_64 : obj = ish_to_met
intel_64 : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) $(LFLAGS) -o $(obj) $(obj_serial)

# Gnu compiler:
gnu_64 : FFLAGS = -O3 -ffree-line-length-0 -cpp
gnu_64 : FCOMPL = gfortran
gnu_64 : obj = ish_to_met
gnu_64 : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(obj_serial)

# *** Object Dependencies ***

ish_to_met.o : ish_to_met.f90

#*** Clean Target to remove Object and Module files ***

.PHONY : clean
clean:
	-rm -f *.o *.mod *.obj 
