SHELL = /bin/sh

#LIBNAME = librmnbeta_14.819u
LIBNAME = librmnbeta_015_953u

WORKDIR = ./WorkDir

LIBDIR = `pwd`/lib/

DEBUG = No

default: genlib

genlib: 
	mkdir -p $(WORKDIR)
	mkdir -p $(LIBDIR)/$(EC_ARCH)
	if [ $(DEBUG) == "yes" ] ; \
	then \
	echo 'Compiling with DEBUG option' ; \
	sleep 2 ; \
	./make_locallib_packages-d ; \
	rm -f $(LIBDIR)/$(EC_ARCH)/$(LIBNAME)_d.a ; \
	./merge_rmnlib_packages $(WORKDIR) $(LIBDIR) $(LIBNAME)_d ; \
	else \
	./make_locallib_packages ; \
	rm -f $(LIBDIR)/$(EC_ARCH)/$(LIBNAME).a ; \
	./merge_rmnlib_packages $(WORKDIR) $(LIBDIR) $(LIBNAME) ; \
	fi
#export CC=gcc ; \

clean:
	cd template_utils/gmm ; make veryclean
	find . -name '*.o' -exec rm {} \;

distclean: clean
	find . -name lib_local.a -exec rm {} \;
