


# who        when       what
# --------  ----------  ----------------------------------------------
# mschilli 2003-11-05  created


#******************************************************************************
# This Makefile follows ALMA Standards (see Makefile(5) for more).
#******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

# 
# IDL Files and flags
# 
IDL_FILES = DemoCallbackResponderIF
IDL_TAO_FLAGS =
USER_IDL =
DemoCallbackResponderIFStubs_LIBS = xmlentityStubs baciStubs acscomponentStubs
#
# Jarfiles and their directories
#
JARFILES = acsCallbacksTest
acsCallbacksTest_DIRS = alma

DEBUG = on
COMPONENT_HELPERS=


#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = 
SCRIPTS_L       = acscallbackTATPrologue \
	  	  CallbacksTest \
		  acscallbackTATEpilogue

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
