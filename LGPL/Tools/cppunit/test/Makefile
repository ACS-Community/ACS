#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.6 2008/10/31 15:27:50 agrimstrup Exp $"
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

PLATFORM := $(shell uname)
CPPUNIT_VER = 1.12.1
CPPUNIT_TAR = 1.12.1

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = cppunitExampleTestCase cppunitExampleTestFailures
EXECUTABLES_L   = 

cppunitExampleTestCase_OBJECTS            = cppunitExampleTestCase
cppunitExampleTestCase_LIBS               = dl cppunit C++

cppunitExampleTestFailures_OBJECTS            = cppunitExampleTestFailures
cppunitExampleTestFailures_LIBS               = dl cppunit C++

#
# Scripts (public and local)
# ----------------------------
SCRIPTS_L         = cppunit_test.sh

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
	@echo " . . . 'all' done" 

clean : clean_all 
	-$(AT) rm -f cppunit_test.log makeCppunitForTest.log
	@echo " . . . clean done";\

clean_dist : clean clean_dist_all 
	@echo " . . . clean_dist done"

#___oOo___
