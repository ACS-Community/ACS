#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id$"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# hsommer  2004-03-15  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# user definable C-compilation flags
#USER_CFLAGS = 

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         =
SCRIPTS_L       = mastercompTATPrologue mastercompTATEpilogue junitNoACS junitWithACS


# 
# IDL Files and flags
# 
IDL_FILES = mastercompTest_if
IDL_TAO_FLAGS =
USER_IDL =
mastercompTest_ifStubs_LIBS = mastercomp_ifStubs

#
# Jarfiles and their directories
# Note that a clean separation between test component impl classes in mastercompTestComp.jar and JUnit test classes in mastercompTest
# would require moving some of the files into different packages. Thus at the moment we have a lot of overlap between the jar files. 
#
JARFILES= mastercompTest
mastercompTest_DIRS=alma
mastercompTest_EXTRAS= 

COMPONENTS_JARFILES = mastercompTestComp
mastercompTestComp_DIRS = \
	alma/ACS/MasterComponentImpl \
	alma/ACS/SpecialTestMasterComponentImpl

#
# java sources in Jarfile on/off
DEBUG=on

#
# ACS XmlIdl generation on/off
#
XML_IDL= 
#
# Java Component Helper Classes generation on/off
#
COMPONENT_HELPERS=
#
# Java Entity Classes generation on/off
#
XSDBIND=
#
# Schema Config files for the above
#
XSDBIND_INCLUDE=
#
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
all:	do_all
	@mkdir -p tmp
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) *~ ../idl/*~ ../*~ *.rep *.out core tmp
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#___oOo___
