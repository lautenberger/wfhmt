VPATH = ../../source/

# Definition of object variables

obj_serial = vars.o \
	fire_dynamics_vars.o \
	subs.o \
	io.o \
	fire_dynamics_io.o \
	fire_dynamics_modules.o \
	fire_dynamics.o \

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
intel_64 : FFLAGS = -m64 -axCORE-AVX2 -O3 -ipo -no-prec-div -fpp -assume byterecl -qopenmp -liomp5
intel_64 : LFLAGS = -static-intel
intel_64 : FCOMPL = ifort
intel_64 : obj = fire_dynamics
intel_64 : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) $(LFLAGS) -o $(obj) $(obj_serial)

# Intel Compiler:
intel_64_debug : FFLAGS = -O0 -traceback -g -check all -fpp -assume byterecl -qopenmp -liomp5
intel_64_debug : LFLAGS = -static-intel
intel_64_debug : FCOMPL = ifort
intel_64_debug : obj = fire_dynamics_debug
intel_64_debug : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) $(LFLAGS) -o $(obj) $(obj_serial)

# Gnu compiler:
gnu_64 : FFLAGS = -O3 -ffree-line-length-0 -cpp
gnu_64 : FCOMPL = gfortran
gnu_64 : obj = fire_dynamics
gnu_64 : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(obj_serial)

# *** Object Dependencies ***

vars.o : vars.f90
fire_dynamics_vars.o : fire_dynamics_vars.f90
subs.o : subs.f90 vars.f90
io.o : io.f90 vars.f90
fire_dynamics_io.o : fire_dynamics_io.f90 fire_dynamics_vars.f90
fire_dynamics_modules.o : fire_dynamics_modules.f90 fire_dynamics_vars.f90
fire_dynamics.o : fire_dynamics.f90 fire_dynamics_vars.f90 fire_dynamics_io.f90 fire_dynamics_modules.f90 subs.f90 io.f90 vars.f90

#*** Clean Target to remove Object and Module files ***

.PHONY : clean
clean:
	-rm -f *.o *.mod *.obj 
