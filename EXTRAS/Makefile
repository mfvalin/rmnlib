#distribution directory, .. should be safe, live dist is often ../..
DISTDIR=..
# extras with respect to RPN's version of rmnlib (14)
# EXTRAS  : load in local library
EXTRAS = fstd98.o DlInterface.o convert_ip123.o fst_missing.o register_dl_routines.o dlfcn.o
# EXTRAS2 : do not load yet
EXTRAS2 = float_packer.o  c_zfstlib.o vmm.o
TESTS = test_z test_p test_f test_d test_c

#RMNLIB = librmnbeta_14.819u.a
RMNLIB = librmn_014.826u.a
all:	obj

# dynamic loader interface
DlInterface.o:	DlInterface.c
	s.cc -c DlInterface.c

register_dl_routines.o: register_dl_routines.c
	s.cc -c -O 2 $<

dlfcn.o: dlfcn.f90
	s.f90 -c $<

# "virtual" memory manager
vmm.o: vmm.c
	s.cc -c -O 2 $<

# level 2 "turbo" compressor
c_zfstlib.o: c_zfstlib.c
	s.cc -c -O 2 $<

# level 1 "F" floating point compressor
float_packer.o: float_packer.c
	s.cc -c -O 2 $<

# missing data points encoding in standard files
fst_missing.o: fst_missing.c
	s.cc -c -O 2 fst_missing.c

fstd98.o: fstd98.c  if_fstd98.c
	s.cc -c -O 2 fstd98.c

# IP123 conversion
convert_ip123.o:	convert_ip123.f90
	s.f90 -c -O 2 convert_ip123.f90

localdist: $(EXTRAS) $(EXTRAS2)
	mkdir -p Locallib/$(EC_ARCH)/
	ar rcv Locallib/$(EC_ARCH)/$(RMNLIB) $(EXTRAS) $(EXTRAS2)
	mkdir -p Localinclude/$(EC_ARCH)/
	mv *.mod Localinclude/$(EC_ARCH)/
	cp convert_ip.h Localinclude/
	rm -f *.o *.mod

dist: $(EXTRAS)
	mkdir -p $(DISTDIR)/lib/$(EC_ARCH)
	ar rcv $(DISTDIR)/lib/$(EC_ARCH)/$(RMNLIB) $(EXTRAS)
	mkdir -p $(DISTDIR)/include/$(EC_ARCH)
	mv *.mod $(DISTDIR)/include/$(EC_ARCH)/
	cp convert_ip.h $(DISTDIR)/include/convert_ip.h_new
	mv $(DISTDIR)/include/convert_ip.h_new $(DISTDIR)/include/convert_ip.h
	rm -f $(EXTRAS)

tests:	$(TESTS)

libshared.so: shared.f90
	s.f90 -shared -o $@ $<

# Fortran dynamic loader interface test
test_d: DlInanterface.o  dlfcn.o register_dl_routines.o libshared.so test_dlfcn.f90
	s.f90 -o $@ test_dlfcn.f90 DlInterface.o  dlfcn.o register_dl_routines.o -ldl

# missing data feaure for standard files test
test_f:	fstd98.c  if_fstd98.c fst_missing.c
	s.cc -c -O 2 -DSELFTEST fstd98.c fst_missing.c
	s.f90 -o $@ test_missing.f90 fstd98.o fst_missing.o -lrmnbeta_014

test_ff: fstd98.c  if_fstd98.c fst_missing.c test_missing.f90
	s.cc -c -O 2 fstd98.c fst_missing.c
	echo "call test_missing_values ; stop ; end" >test_missing_main.f90
	s.f90 -o $@ test_missing_main.f90 test_missing.f90 fstd98.o fst_missing.o -lrmnbeta_014
	rm test_missing_main.f90

# F type floating point packer test
test_p:	float_packer.c
	s.cc -o $@ -O 3 -DTEST_PACK $< -lm

# stage 2 compression/decompression tests with timing
test_z:	c_zfstlib.c
	s.cc -o $@ -O 3 -DTEST_TURBO c_zfstlib.c -lm

# IP123 conversion tests
test_c:	convert_ip123.o test_c_convip.c test_f_convip.f90
	s.cc -c test_c_convip.c
	s.f90 -o $@ test_f_convip.f90 test_c_convip.o convert_ip123.o

obj: $(EXTRAS) $(EXTRAS2)
#	rm -f *.o *.mod

clean:
	rm -f *.o *.so *.mod $(TESTS) a.out

distclean: clean
	rm -rf Locallib Localinclude
