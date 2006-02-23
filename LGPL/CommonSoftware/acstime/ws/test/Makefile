#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.17 2005/01/31 21:45:24 dfugate Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi 2003-09-01 Removed test target
# dave  2003-07-28  created
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
USER_CFLAGS =
USER_LIB = -lACE \
           -lTAO \
           -lTAO_DsLogAdmin \
           -lTAO_CosNaming \
           -lTAO_IORTable \
           -lTAO_PortableServer \
           -lTAO_Svc_Utils \
           -lTAO_CosTrading \
           -lTAO_DynamicAny \
           -lTAO_IFR_Client \
           -lacsutil \
           -lcdb \
           -llogging \
	   -lacscomponent \
           -lbaci \
           -lmaci \
           -lmaciClient \
           -lacstime

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     =
EXECUTABLES_L   = testClock testDuration testEpoch testNow testProp \
		  testTimeout testTimeUtil timeoutExample \
		  testProfiler  #testException

#
# <brief description of xxxxx program>
xxxxx_OBJECTS   =	
xxxxx_LDFLAGS   =
xxxxx_LIBS      =

testClock_OBJECTS = testClock
testClock_LIBS    = 

testDuration_OBJECTS = testDuration
testDuration_LIBS    = 

testEpoch_OBJECTS = testEpoch
testEpoch_LIBS    = 

testException_OBJECTS = testException
testException_LIBS    = timeServiceC maciClient 

testNow_OBJECTS = testNow
testNow_LIBS    = 

testProp_OBJECTS = testProp
testProp_LIBS    = 

testTimeout_OBJECTS = testTimeout
testTimeout_LIBS    = 

timeoutExample_OBJECTS = timeoutExample
timeoutExample_LIBS    = 

testTimeUtil_OBJECTS = testTimeUtil
testTimeUtil_LIBS    = 

testProfiler_OBJECTS = testProfiler
testProfiler_LIBS    = 



#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

#
# Includes (.h) files (public only)
# ---------------------------------
INCLUDES        =

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       =
LIBRARIES_L     =

#
# <brief description of lllll library>
lllll_OBJECTS   =

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         =
SCRIPTS_L       = acstimeTATPrologue

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS   =
TCL_SCRIPTS_L   = 


#maciTest_OBJECTS = maciTest
#maciTest_TCLSH   = expect

#
# Python stuff (public and local)
# ----------------------------
PY_SCRIPTS         =
PY_SCRIPTS_L       =

PY_MODULES         =
PY_MODULES_L       =

PY_PACKAGES        =
PY_PACKAGES_L      =
pppppp_MODULES	   =

#
# <brief description of tttttt tcl-script>
tttttt_OBJECTS  =
tttttt_TCLSH    = 
tttttt_LIBS     = 

#
# TCL libraries (public and local)
# ------------------------------
TCL_LIBRARIES   =
TCL_LIBRARIES_L =

#
# <brief description of tttlll library>
tttlll_OBJECTS  = 

#
# UIF panels (public and local)
# ----------------------------
PANELS   =
PANELS_L = 

# 
# IDL Files and flags
# 
IDL_FILES =
IDL_TAO_FLAGS =
USER_IDL =
#
# Jarfiles and their directories
#
JARFILES= 
jjj_DIRS=
jjj_EXTRAS= 
#
# java sources in Jarfile on/off
DEBUG= 
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
# man pages to be done
# --------------------
MANSECTIONS =
MAN1 =
MAN3 =
MAN5 =
MAN7 =
MAN8 =

#
# local man pages
# ---------------
MANl =

#
# ASCII file to be converted into Framemaker-MIF
# --------------------
ASCII_TO_MIF = 

#
# INS_ROOT files to be installed
#-------------------------------
INS_ROOT_FILES =
INS_ROOT_DIR   =

#
# other files to be installed
#----------------------------
INSTALL_FILES =

#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

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
	$(RM) *~
	$(RM) ref/*~ tatlogs
	$(RM) sed.scan
	$(RM) tmp
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"


#___oOo___
