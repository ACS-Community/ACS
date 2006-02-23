#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.6 2005/09/10 11:13:33 gchiozzi Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# almamgr  12/12/03  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

SHELL=/bin/bash
PLATFORM := $(shell uname)
MIMETIC_VER = 0.8.9

#
# Scripts (public and local)
# ----------------------------
#SCRIPTS_L         = configure_script

#
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
all:	unpack configure build
	@echo " . . . 'all' done" 

clean :
	-$(AT) rm -rf  mimetic-$(MIMETIC_VER) configure_script.log
	@echo " . . . clean done";\

clean_dist : clean
	@echo " . . . clean_dist done"

man   :
	@echo " . . . nothing to do for man page(s)"

install :
	@cd mimetic-$(MIMETIC_VER); make install
	@echo " . . . installation done"

unpack :
	@echo ". . . unpacking the tar files . . . "
	@gtar -xzf mimetic-$(MIMETIC_VER).tar.gz

configure :
	@echo ". . . running configure . . . "
	@chmod ug+x configure_script
	@./configure_script mimetic-$(MIMETIC_VER).tar.gz $(MIMETIC_VER) > configure_script.log 2>&1

build :
	@echo ". . . building . . . "
	@cd mimetic-$(MIMETIC_VER); make all
#___oOo___
