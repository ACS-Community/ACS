#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id: Makefile,v 1.3 2011/11/28 16:22:27 javarias Exp $"
#
# Makefile of ........
#
# who         when      what
# --------    --------  ----------------------------------------------
# agrimstrup  30/09/09  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

SHELL=/bin/bash
SHUNIT_VER = 2.1.5

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
all:	do_all unpack build
	@echo " . . . 'all' done" 

clean : clean_all
	@if [ -d shunit2-$(SHUNIT_VER) ]; then cd shunit2-$(SHUNIT_VER); make build-clean; fi
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo "\n . . . removing the shunit2 directory . . ."
	@rm -rf shunit2-$(SHUNIT_VER)
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install :
	@echo ". . . installing shunit2 . . . "
	@if [ ! -z "$(INTROOT)" ]; \
	then \
		cp shunit2-$(SHUNIT_VER)/build/shunit2 $(INTROOT)/bin ; \
	else \
		cp shunit2-$(SHUNIT_VER)/build/shunit2 $(ACSROOT)/bin ; \
	fi
	@echo " . . . installation done"

unpack:
	@ if [ ! -d shunit2-$(SHUNIT_VER) ]; \
          then \
            echo ". . . unpacking the tar files . . . "; \
            tar -xzf shunit2-$(SHUNIT_VER).tgz; \
          fi

build:
	@echo ". . . building . . . "
	@cd shunit2-$(SHUNIT_VER); $(MAKE) $(MAKE_PARS) build

#___oOo___
