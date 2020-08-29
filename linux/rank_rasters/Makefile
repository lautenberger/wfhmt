VPATH = ../../source/

# Definition of object variables

obj_serial = vars.o \
	sort.o \
	subs.o \
	io.o \
	rank_rasters.o \

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
intel_64 : obj = rank_rasters
intel_64 : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) $(LFLAGS) -o $(obj) $(obj_serial)

# Intel Compiler:
intel_64_openmpno : FFLAGS = -m64 -axCORE-AVX2 -O3 -ipo -no-prec-div -fpp -assume byterecl
intel_64_openmpno : LFLAGS = -static-intel
intel_64_openmpno : FCOMPL = ifort
intel_64_openmpno : obj = rank_rasters_openmpno
intel_64_openmpno : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) $(LFLAGS) -o $(obj) $(obj_serial)

# Gnu compiler:
gnu_64 : FFLAGS = -O3 -ffree-line-length-0 -cpp -fopenmp
gnu_64 : FCOMPL = gfortran
gnu_64 : obj = rank_rasters
gnu_64 : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(obj_serial)

# Gnu compiler:
gnu_64_openmpno : FFLAGS = -O3 -ffree-line-length-0 -cpp
gnu_64_openmpno : FCOMPL = gfortran
gnu_64_openmpno : obj = rank_rasters_openmpno
gnu_64_openmpno : setup $(obj_serial)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(obj_serial)

# *** Object Dependencies ***

vars.o : vars.f90
sort.o : sort.for
subs.o : subs.f90 vars.f90
io.o : io.f90 vars.f90
rank_rasters.o : rank_rasters.f90 subs.f90 io.f90 vars.f90 sort.for

#*** Clean Target to remove Object and Module files ***

.PHONY : clean
clean:
	-rm -f *.o *.mod *.obj 
