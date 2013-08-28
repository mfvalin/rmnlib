EXTRAS = fstd98.o DlInterface.o convert_ip123.o

RMNLIB = librmnbeta_14.819u.a
all:	obj

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
	ar rcv lib/$(EC_ARCH)/librmnbeta_014u.a $(EXTRAS)
	mv convert_ip123.mod include/$(EC_ARCH)/convert_ip123.mod
	cp convert_ip.h include/$(EC_ARCH)/convert_ip.h_new
	mv include/$(EC_ARCH)/convert_ip.h_new include/$(EC_ARCH)/convert_ip.h
	rm -f *.o *.mod

obj: $(EXTRAS)
	rm -f *.o *.mod

clean:
	rm -f *.o *.mod

