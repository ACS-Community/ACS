#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id: Makefile,v 1.1 2010/10/27 15:44:57 htischer Exp $"
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

#MAKEDIRTMP := $(shell searchFile include/acsMakefile)
#ifneq ($(MAKEDIRTMP),\#error\#)
#   MAKEDIR := $(MAKEDIRTMP)/include
#   include $(MAKEDIR)/acsMakefile
#endif

#
# TARGETS
# -------
all: unpack patch configure build
	@echo " . . . 'all' done" 

clean : 
ifneq ($(PLATFORM),Linux)
	-$(AT) if [ -d log4cpp-$(LOG4CPP_VER) ]; then cd log4cpp-$(LOG4CPP_VER) ; $(MAKE) clean > /dev/null ; fi
endif
	@rm -rf log4cpp-$(LOG4CPP_VER)
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   :
	$(MAKE) -C log4cpp-$(LOG4CPP_VER)/doc all install
	@echo " . . . man page(s) done"

install : 
	@echo ". . . installing cppunit . . ."
	@cd log4cpp-$(LOG4CPP_VER); $(MAKE) install
	@echo " . . . installation done"

.PHONY:unpack
unpack :
	@ if [ ! -d log4cpp-$(LOG4CPP_VER) ]; \
		then \
		echo ". . . unpacking the tar files . . . "; \
		rm -rf last_patched; \
		gtar -xzf log4cpp-$(LOG4CPP_TAR).tar.gz; \
		fi

patch: unpack last_patched

last_patched: unpack log4cpp.patch
	@cd log4cpp-$(LOG4CPP_VER); patch -p1 < ../log4cpp.patch
	@touch last_patched
	@echo " . . . patch applied";\

.PHONY: configure
configure : patch unpack
	@mkdir -p ../bin
	@echo ". . . running configure . . . "
	@cp configure_script ../bin/configure_script
	@chmod +x ../bin/configure_script	
	@../bin/configure_script log4cpp-$(LOG4CPP_TAR).tar.gz $(LOG4CPP_VER) > configure_script.log 2>&1

build : configure
	@echo ". . . building . . . "
	@cd log4cpp-$(LOG4CPP_VER); $(MAKE) all

#___oOo___
