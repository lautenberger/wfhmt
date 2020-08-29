VPATH = ../../source/

# Definition of object variables

obj_serial = vars.o \
	sort.o \
	subs.o \
	io.o \
	raster_percentiles.o \

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
intel_64 : obj = raster_percentiles
intel_64 : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) $(LFLAGS) -o $(obj) $(obj_serial)

intel_64_debug : FFLAGS = -O0 -g -traceback -fpp -assume byterecl
intel_64_debug : LFLAGS = -static-intel
intel_64_debug : FCOMPL = ifort
intel_64_debug : obj = raster_percentiles_debug
intel_64_debug : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) $(LFLAGS) -o $(obj) $(obj_serial)

# Gnu compiler:
gnu_64 : FFLAGS = -O3 -ffree-line-length-0 -cpp
gnu_64 : FCOMPL = gfortran
gnu_64 : obj = raster_percentiles
gnu_64 : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(obj_serial)

# *** Object Dependencies ***

vars.o : vars.f90
sort.o : sort.for
subs.o : subs.f90 vars.f90
io.o : io.f90 vars.f90
raster_percentiles.o : raster_percentiles.f90 subs.f90 io.f90 vars.f90 sort.for

#*** Clean Target to remove Object and Module files ***

.PHONY : clean
clean:
	-rm -f *.o *.mod *.obj 
