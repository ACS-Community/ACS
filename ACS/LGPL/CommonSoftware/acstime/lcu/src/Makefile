#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.7 2006/09/11 08:07:47 bjeram Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi 2003-09-01 Removed test target
# dave  23/07/03  created
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
# additional include and library search paths
#USER_INC = 
#USER_LIB = 

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = acstime acsclock acstimer
EXECUTABLES_L   = 

#
# <brief description of xxxxx program>
xxxxx_OBJECTS   =	
xxxxx_LDFLAGS   =
xxxxx_LIBS      =

acstime_OBJECTS =  acstimeTimeUtil \
		   acstimeDurationHelper acstimeEpochHelper \
	           acstimeProfiler \
		   acstimeC acstimeS

acsclock_OBJECTS = acstimeClockImpl 

acstimer_OBJECTS = acstimeTimerImpl 


#
# IDL Files and flags
#
IDL_FILES = acstime
IDL_TAO_FLAGS =
USER_IDL =

#ACS Error System target
ACSERRDEF = ACSTimeError

#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

#
# handle links to common source files (in MODNAME/ws/src)
#
LINK_FILES= $(addprefix ../idl/, $(addsuffix .midl, $(IDL_FILES))) \
            $(addprefix ../idl/, $(addsuffix .xml, $(ACSERRDEF))) \
             acstimeClockImpl.cpp \
	     acstimeTimerImpl.cpp \
	     acstimeTimeUtil.cpp \
	     acstimeProfiler.cpp \
	     acstimeDurationHelper.cpp acstimeEpochHelper.cpp


#
#>>>>> END OF standard rules

#
# CHOSE PLATFORM
# --------------
# This is a LCU Makefile, so for VxVorks applications we set:
MAKE_VXWORKS = on   

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
all:	do_links do_all
	@echo " . . . 'all' done" 

clean : clean_all
	$(RM) ../idl/acstime.idl
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#___oOo___
