#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.27 2009/10/14 11:33:21 hsommer Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# dfugate  14/09/04  created
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
USER_INC = -I$(TAO_ROOT)/orbsvcs/orbsvcs/AV
USER_LIB = 	-lACE \
		-lTAO_AV \
		-lTAO \
		-lTAO_DsLogAdmin \
		-lTAO_CosNaming \
		-lTAO_IORTable \
		-lTAO_PortableServer \
		-lTAO_Svc_Utils \
		-lTAO_CosTrading \
                -lTAO_CosNotification \
		-lTAO_DynamicAny \
		-lTAO_IFR_Client \
                -lTAO_CosProperty \
		-lacsutil \
		-lcdb \
		-llogging \
	        -lacscomponent \
		-lbaci \
		-lmaci \
		-lmaciClient \
		-lacserr \
		-lm \
		-lbulkDataSenderStubs \
		-lbulkDataStubs \
		-lACSBulkDataError \
		-lbulkDataReceiverStubs

ACS_VERSION = $(shell cat $(ACSROOT)/ACS_VERSION)
#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = 
EXECUTABLES_L   = 	componentsBulkDataTest \
			componentsGenericTest componentsClientErrTest componentsClientMethodTest componentsClientSimpleTest 

#
# <brief description of xxxxx program>
xxxxx_OBJECTS   =	
xxxxx_LDFLAGS   =
xxxxx_LIBS      =

componentsBulkDataTest_OBJECTS = componentsBulkDataTest
componentsBulkDataTest_LIBS    = maciClient maci BenchmarkErrType bulkDataCallback ACSBulkDataError perftestStubs perftestBDStubs

componentsGenericTest_OBJECTS = componentsGenericTest
componentsGenericTest_LIBS    = maciClient maci BenchmarkErrType perftestStubs

componentsClientSimpleTest_OBJECTS = componentsClientSimpleTest
componentsClientSimpleTest_LIBS    = maciClient maci BenchmarkErrType acstime perftestStubs

componentsClientMethodTest_OBJECTS = componentsClientMethodTest
componentsClientMethodTest_LIBS    = maciClient maci BenchmarkErrType acstime perftestStubs


componentsClientErrTest_OBJECTS = componentsClientErrTest
componentsClientErrTest_LIBS    = maciClient maci BenchmarkErrType acstime perftestStubs

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
SCRIPTS         = monitorProcess monitorPid.sh remoteMonitorProcess
SCRIPTS_L       = 	genMakeReport genCompReport \
			genCompLoggingReport \
			genCompEventReport \
			genCompErrorReport \
			genCompMethodReport \
			genCompBulkDataReport \
			genLoggingProcessesReport \
			genStartupTest \
			calculatePerformance \
			loggingPerformance \
			sleepRunner

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     =
TCL_SCRIPTS_L   =

#
# Python stuff (public and local)
# ----------------------------
PY_SCRIPTS         = NotificationServiceTest
PY_SCRIPTS_L       = componentsGenericTestPy componentsClientTestPy componentsMethodTestPy \
			componentsClientEventConsumer

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
# Configuration Database Files
# ----------------------------
CDB_SCHEMAS = 

# 
# IDL Files and flags
# 
IDL_FILES =
IDL_TAO_FLAGS =
USER_IDL =
#
# Jarfiles and their directories
#
JARFILES= perftestClient 
perftestClient_DIRS = alma

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
# other files to be installed
#----------------------------
INSTALL_FILES =

#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach rtos, $(RTAI_MODULES) , $($(rtos)_OBJECTS)) \
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
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) *~ *.pyc
	$(RM) tmp
	$(RM) .idl-compilation-*
	@# clean-up the copied reference files
	$(RM) ref/cppPerformance.ref ref/javaPerformance.ref ref/pythonPerformance.ref
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

clean_dbm:
	$(RM) ../doc/comp.dbm
	$(RM) ../doc/compLogging.dbm
	$(RM) ../doc/compEvent.dbm
	$(RM) ../doc/compError.dbm
	$(RM) ../doc/compMethod.dbm
	$(RM) ../doc/compBulkData.dbm

#___oOo___
