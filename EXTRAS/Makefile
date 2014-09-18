#distribution directory, .. should be safe, live dist is often ../..
DISTDIR=..
# extras with respect to RPN's version of rmnlib (14)
# EXTRAS  : load in local library
EXTRAS = fstd98.o  fst_missing.o  xdf98.o
# EXTRAS2 : do not load yet
EXTRAS2 = float_packer.o  c_zfstlib.o vmm.o excdes_new.o mgilib2.o mgi_watch.o mgi_test.o mgi_shm.o
TESTS = test_z test_p test_f test_ff test_d test_c test_excdes

#RMNLIB = librmnbeta_14.819u.a
#RMNLIB = librmn_014.826u.a
#BASE_RMNLIB := rmn_014
BASE_RMNLIB := rmnbeta_015_959u
NEW_RMNLIB := rmnbeta_016
TEST_RMNLIB := Locallib/$(EC_ARCH)/lib$(NEW_RMNLIB).a
EXTRA_LOCAL := -ILocalinclude -ILocalinclude/$(EC_ARCH) -LLocallib/$(EC_ARCH) -l$(NEW_RMNLIB)
EXTRA_LOCAL_R := -ILocalinclude -ILocalinclude/$(EC_ARCH) -LLocallib/$(EC_ARCH) -l$(BASE_RMNLIB)
all:	obj

# dynamic loader interface
mgilib2.o:	mgilib2.c
	s.cc -c -O 2 mgilib2.c

mgi_watch.o:	mgi_watch.c
	s.cc -c -O 2 $<

mgi_test.o:	mgi_test.c
	s.cc -c -O 2 $<

mgi_shm.o:	mgi_shm.c
	s.cc -c -O 2 $<

excdes_new.o:	excdes_new.c
	s.cc -c -O 2 $<

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

xdf98.o: xdf98.c
	s.cc -c -O 2 xdf98.c

fstd98.o: fstd98.c  if_fstd98.c
	s.cc -c -O 2 fstd98.c

$(TEST_RMNLIB): $(EXTRAS) $(EXTRAS2)
	mkdir -p Locallib/$(EC_ARCH)/
#	s.locate --lib $(BASE_RMNLIB) && cp $$(s.locate --lib $(BASE_RMNLIB)) $(TEST_RMNLIB)
	cp Locallib/$(EC_ARCH)/lib$(BASE_RMNLIB).a $(TEST_RMNLIB)
	mkdir -p Localinclude/$(EC_ARCH)/
	touch dummy.mod dummy.inc
	cp *.mod Localinclude/$(EC_ARCH)/
	cp *.inc Localinclude/
#	cp convert_ip.h Localinclude/
#	ar d $(TEST_RMNLIB) excdes.o
	ar rcv $(TEST_RMNLIB) $(EXTRAS) $(EXTRAS2)


localdist: $(TEST_RMNLIB)
#	rm -f *.o *.mod

dist: $(EXTRAS)
	mkdir -p $(DISTDIR)/lib/$(EC_ARCH)
	ar rcv $(DISTDIR)/lib/$(EC_ARCH)/lib$(NEW_RMNLIB).a $(EXTRAS)
	mkdir -p $(DISTDIR)/include/$(EC_ARCH)
	cp *.mod $(DISTDIR)/include/$(EC_ARCH)/
#	cp convert_ip.h $(DISTDIR)/include/convert_ip.h_new
#	mv $(DISTDIR)/include/convert_ip.h_new $(DISTDIR)/include/convert_ip.h
	rm -f $(EXTRAS)

tests:	$(TESTS)

libshared.so: shared.f90
	s.f90 -shared -o $@ $<

# Fortran dynamic loader interface test
test_d: $(TEST_RMNLIB) libshared.so test_dlfcn.f90
	s.f90 -o $@ test_dlfcn.f90 $(EXTRA_LOCAL) -ldl

# missing data feature for standard files test
test_f:	fst_missing.c  $(TEST_RMNLIB)
	rm -f $@
	s.cc -o $@ -O 2 -DSELFTEST fst_missing.c $(EXTRA_LOCAL)
#	s.f90 -o $@ test_missing.f90 fstd98.o fst_missing.o $(EXTRA_LOCAL)
#	rm -f fstd98.o fst_missing.o

test_ff: $(TEST_RMNLIB) test_missing.f90
#	s.cc -c -O 2 fstd98.c fst_missing.c
	echo "call test_missing_values ; stop ; end" >test_missing_main.f90
	s.f90 -o $@ test_missing_main.f90 test_missing.f90 $(EXTRA_LOCAL)
	s.f90 -o $@_R test_missing_main.f90 test_missing.f90 $(EXTRA_LOCAL_R)
	rm test_missing_main.f90
	rm -f missing.fst

test_excdes: $(TEST_RMNLIB) test_excdes.f90 excdes_new.c
	s.cc -c excdes_new.c -DTEST -Dc_main=c_main_
	s.f90 test_excdes.f90 excdes_new.o $(EXTRA_LOCAL) -o $@
	rm -f excdes_new.o

test_dlfcn: test_dlfcn.f90 $(TEST_RMNLIB)
	s.f90 -o $@ test_dlfcn.f90 $(EXTRA_LOCAL)

# F type floating point packer test
test_p:	float_packer.c
	s.cc -o $@ -O 3 -DTEST_PACK $< -lm

# stage 2 compression/decompression tests with timing
test_z:	c_zfstlib.c
	s.cc -o $@ -O 3 -DTEST_TURBO c_zfstlib.c -lm

# IP123 conversion tests
test_c:	$(TEST_RMNLIB) test_c_convip.c test_f_convip.f90
	s.cc -c test_c_convip.c
	s.f90 -o $@ test_f_convip.f90 test_c_convip.o $(EXTRA_LOCAL)

mgi_tests: mgi_test mgi_shm mgi_watch f_mgi_test

mgi_test: $(TEST_RMNLIB)
	@echo "int main(int argc, char **argv) { return mgi_test(argc,argv) ; }" >dummy.c
	s.cc -o $@ dummy.c $(EXTRA_LOCAL)

mgi_shm: $(TEST_RMNLIB)
	@echo "int main(int argc, char **argv) { return mgi_shm(argc,argv) ; }" >dummy.c
	s.cc -o $@ dummy.c $(EXTRA_LOCAL)
	rm -f dummy

mgi_watch: $(TEST_RMNLIB)
	@echo "int main(int argc, char **argv) { return mgi_watch(argc,argv) ; }" >dummy.c
	s.cc -o $@ dummy.c $(EXTRA_LOCAL)

f_mgi_test: $(TEST_RMNLIB) f_mgi_test.f90
	s.f90 -o $@  f_mgi_test.f90 $(EXTRA_LOCAL)

obj: $(EXTRAS) $(EXTRAS2)
#	rm -f *.o *.mod

clean:
	rm -f *.o *.so *.mod $(TESTS) a.out missing.fst attach_shm  mgi_shm mgi_watch

distclean: clean
	rm -rf Locallib Localinclude
