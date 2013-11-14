EXTRAS = fstd98.o DlInterface.o convert_ip123.o fst_missing.o
EXTRAS2 = float_packer.o  c_zfstlib.o dlfcn.o register_dl_routines.o
TESTS = test_z test_p test_f test_d

RMNLIB = librmnbeta_14.819u.a
all:	obj

register_dl_routines.o: register_dl_routines.c
	s.cc -c -O 2 $<

dlfcn.o: dlfcn.f90
	s.f90 -c $<

c_zfstlib.o: c_zfstlib.c
	s.cc -c -O 2 $<

float_packer.o: float_packer.c
	s.cc -c -O 2 $<

fst_missing.o: fst_missing.c
	s.cc -c -O 2 fst_missing.c

fstd98.o: fstd98.c  if_fstd98.c
	s.cc -c -O 2 fstd98.c

DlInterface.o:	DlInterface.c
	s.cc -c DlInterface.c

convert_ip123.o:	convert_ip123.f90
	s.f90 -c -O 2 convert_ip123.f90

localdist: $(EXTRAS)
	ar rcv LocalLib/$(EC_ARCH)/$(RMNLIB) $(EXTRAS)
	rm -f *.o *.mod

dist: $(EXTRAS)
	ar rcv lib/$(EC_ARCH)/$(RMNLIB) $(EXTRAS)
	mv convert_ip123.mod include/$(EC_ARCH)/convert_ip123.mod
	cp convert_ip.h include/$(EC_ARCH)/convert_ip.h_new
	mv include/$(EC_ARCH)/convert_ip.h_new include/$(EC_ARCH)/convert_ip.h
	rm -f *.o *.mod

tests:	$(TESTS)

libshared.so: shared.f90
	s.f90 -shared -o $@ $<

test_d: DlInterface.o  dlfcn.o register_dl_routines.o libshared.so test_dlfcn.f90
	s.f90 -o $@ test_dlfcn.f90 DlInterface.o  dlfcn.o register_dl_routines.o -ldl

test_f:	fstd98.c  if_fstd98.c fst_missing.c test_missing.f90
	s.cc -c -O 2 -DSELFTEST fstd98.c fst_missing.c
	s.f90 -o $@ test_missing.f90 fstd98.o fst_missing.o -lrmnbeta_014

test_p:	float_packer.c
	s.cc -o $@ -O 3 -DTEST_PACK $< -lm

test_z:	c_zfstlib.c
	s.cc -o $@ -O 3 -DTEST_TURBO c_zfstlib.c -lm

obj: $(EXTRAS) $(EXTRAS2)
#	rm -f *.o *.mod

clean:
	rm -f *.o *.so *.mod $(TESTS) a.out

