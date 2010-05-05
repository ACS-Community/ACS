#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id: Makefile,v 1.1 2010/05/05 22:18:29 javarias Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# javarias  05/05/10  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------
SHELL=/bin/bash
PLATFORM := $(shell uname)
LOG4CPP_VER = 1.0
LOG4CPP_TAR = 1.0

#
# Scripts (public and local)
# ----------------------------
SCRIPTS_L       = configure_script

#>>>>> END OF standard rules

#
# INCLUDE STANDARDS
# -----------------

MAKEDIRTMP := $(shell searchFile include/acsMakefile)
ifneq ($(MAKEDIRTMP),\#error\#)
   MAKEDIR := $(MAKEDIRTMP)/include
   include $(MAKEDIR)/acsMakefile
endif

#
# TARGETS
# -------
all:	do_all patch unpack configure build
	@echo " . . . 'all' done" 

clean : clean_all 
ifneq ($(PLATFORM),Linux)
	-$(AT) if [ -d log4cpp-$(LOG4CPP_VER) ]; then cd log4cpp-$(LOG4CPP_VER) ; make clean > /dev/null ; fi
endif
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo ". . . installing cppunit . . . "
	@cd log4cpp-$(LOG4CPP_VER); make install
	@echo " . . . installation done"

unpack :
	@ if [ ! -d log4cpp-$(LOG4CPP_VER) ]; \
		then \
		echo ". . . unpacking the tar files . . . "; \
		rm -rf last_patched; \
		gtar -xzf log4cpp-$(LOG4CPP_TAR).tar.gz; \
		fi

patch: last_patched

last_patched: log4cpp.patch
	@cd log4cpp-$(LOG4CPP_VER); patch -p1 < ../log4cpp.patch
	@touch last_patched
	@echo " . . . patch applied";\

configure :
	@echo ". . . running configure . . . "
	@../bin/configure_script log4cpp-$(LOG4CPP_TAR).tar.gz $(LOG4CPP_VER) > configure_script.log 2>&1

build :
	@echo ". . . building . . . "
	@cd log4cpp-$(LOG4CPP_VER); make all

#___oOo___
