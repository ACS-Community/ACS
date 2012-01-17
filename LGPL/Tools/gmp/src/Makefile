# This is a crude ACS compatible makefile for GMP, "borrowed" from the CONTROL team.

#
# INCLUDE STANDARDS
# -----------------
MAKEDIRTMP := $(shell searchFile include/acsMakefile)
ifneq ($(MAKEDIRTMP),\#error\#)
   MAKEDIR := $(MAKEDIRTMP)/include
   include $(MAKEDIR)/acsMakefile
endif

#
# Defines BUILD_PLATFORM_FLAGS assuming pentium3 pc 
# hardware.
# This does not work for 64bit architecture.
# In that case we let config choose the best by itself.
#
BUILD_PLATFORM_FLAGS=
ARCH_RET=$(shell uname -a | grep -c 'x86_64')
ifeq ($(ARCH_RET), 1)
   BUILD_PLATFORM_FLAGS=
else
  ifndef GMP_OPTIMIZE
    ifeq ($(OS), $(CYGWIN_VER))
      BUILD_PLATFORM_FLAGS=--build=pentium3-pc-cygwin
    else
      BUILD_PLATFORM_FLAGS=--build=pentium3-pc-linux-gnu
    endif
  endif
endif
##############################################


# RANLIB is defined by vltMakefile, but the definition prevents
# the configure stage from working properly (leading to failures in
# 'make all' for GMP). Therefore, clear RANLIB here.
RANLIB=
LD=
GMP_VER = 4.1.3

all: unpack config
	$(MAKE) $(MAKE_PARS) -C gmp-$(GMP_VER) all

install:
	$(MAKE) -C gmp-$(GMP_VER) install

clean:
	rm -rf gmp-$(GMP_VER)

check:  unpack config
	$(MAKE) -C gmp-$(GMP_VER) check

unpack:
	@if [ ! -d gmp-$(GMP_VER) ]; then \
           tar zxf gmp-$(GMP_VER).tar.gz; \
           patch gmp-$(GMP_VER)/gmp-impl.h < gmp-impl.patch; \
        fi
config: unpack
	@cd gmp-$(GMP_VER) && \
	./configure $(BUILD_PLATFORM_FLAGS) --prefix=$(INSTALL_ROOT) --enable-cxx
