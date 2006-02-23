# This is a crude ACS compatible makefile for GMP, "borrowed" from the CONTROL team.

#
# INCLUDE STANDARDS
# -----------------
MAKEDIRTMP := $(shell searchFile include/acsMakefile)
ifneq ($(MAKEDIRTMP),\#error\#)
   MAKEDIR := $(MAKEDIRTMP)/include
   include $(MAKEDIR)/acsMakefile
endif

BUILD_PLATFORM_FLAGS=
ifndef GMP_OPTIMIZE
   BUILD_PLATFORM_FLAGS=--build=pentium3-pc-linux-gnu
endif

# RANLIB is defined by vltMakefile, but the definition prevents
# the configure stage from working properly (leading to failures in
# 'make all' for GMP). Therefore, clear RANLIB here.
RANLIB=
LD=
GMP_VER = 4.1.3

all: unpack config
	make -C gmp-$(GMP_VER) all

install:
	make -C gmp-$(GMP_VER) install

clean:
	rm -rf gmp-$(GMP_VER)

check:  unpack config
	make -C gmp-$(GMP_VER) check

unpack:
	@if [ ! -d gmp-$(GMP_VER) ]; then \
           tar zxf gmp-$(GMP_VER).tar.gz; \
        fi
config:
	@cd gmp-$(GMP_VER) && \
	./configure $(BUILD_PLATFORM_FLAGS) --prefix=$(INSTALL_ROOT) --enable-cxx
