#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.28 2011/03/28 11:27:36 jagonzal Exp $"
#
# Makefile of ........
#
# who       when        what
# --------  ----------  ----------------------------------------------
# dfugate   2004-09-16  created
# eallaert  2007-08-27  added fix_idl target
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
		-lloki \
		-lacstime \
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
EXECUTABLES_L   = logMonitorClient logSenderClient loggingClientHack


logSenderClient_OBJECTS=LogSenderClient
logMonitorClient_OBJECTS=LogMonitorClient

loggingClientHack_OBJECTS=loggingClient loggingHelper
loggingClientHack_LIBS = logging

#
# <brief description of xxxxx program>
xxxxx_OBJECTS   =	
xxxxx_LDFLAGS   =
xxxxx_LIBS      =

#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

#
# Includes (.h) files (public only)
# ---------------------------------
INCLUDES        =  	IdlCompilationTimeImpl.h BasePerfCompImpl.h \
			SimpleBACIComponentImpl.h \
			ComplexBACIComponentImpl.h \
			LogTestComponentImpl.h \
			ErrTestComponentImpl.h \
			SupplierTestComponentImpl.h \
			componentsBulkReceiver.h componentsBulkSender.h LogStressImpl.h

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       = 	componentsBulkReceiver componentsBulkSender \
			MethodTestComponentImpl SimpleBACIComponentImpl ComplexBACIComponentImpl \
			LogTestComponentImpl ErrTestComponentImpl SupplierTestComponentImpl LogStressImpl

LIBRARIES_L     =

LogStressImpl_OBJECTS = LogStressImpl
LogStressImpl_LIBS	= perftestStubs BenchmarkErrType

componentsBulkReceiver_OBJECTS   = componentsBulkReceiver 
componentsBulkReceiver_LIBS      =  bulkDataReceiverStubs bulkDataStubs bulkDataCallback ACSBulkDataError 

componentsBulkSender_OBJECTS   = componentsBulkSender
componentsBulkSender_LIBS      = bulkDataReceiverStubs bulkDataSenderStubs bulkDataStubs ACSBulkDataError perftestBDStubs

SimpleBACIComponentImpl_OBJECTS	= SimpleBACIComponentImpl
SimpleBACIComponentImpl_LIBS	= perftestStubs BenchmarkErrType

ComplexBACIComponentImpl_OBJECTS	= ComplexBACIComponentImpl
ComplexBACIComponentImpl_LIBS		= perftestStubs BenchmarkErrType

LogTestComponentImpl_OBJECTS	= LogTestComponentImpl
LogTestComponentImpl_LIBS	= perftestStubs BenchmarkErrType

ErrTestComponentImpl_OBJECTS	= ErrTestComponentImpl
ErrTestComponentImpl_LIBS	= perftestStubs BenchmarkErrType

SupplierTestComponentImpl_OBJECTS	= SupplierTestComponentImpl
SupplierTestComponentImpl_LIBS		= perftestStubs BenchmarkErrType acsnc

MethodTestComponentImpl_OBJECTS	= MethodTestComponentImpl
MethodTestComponentImpl_LIBS	= perftestStubs BenchmarkErrType



# <brief description of lllll library>
lllll_OBJECTS   =

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         =
SCRIPTS_L       =

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     =
TCL_SCRIPTS_L   =

#
# Python stuff (public and local)
# ----------------------------
PY_SCRIPTS         = NCStressConsumer NCStressSupplier
PY_SCRIPTS_L       =

PY_MODULES         =
PY_MODULES_L       =

PY_PACKAGES        = perftestImpl
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
IDL_FILES = perftest IdlCompilationTime perftestBD 
IDL_TAO_FLAGS =
USER_IDL = -I$(ACE_ROOT)/TAO/orbsvcs/orbsvcs/

ACSERRDEF = BenchmarkErrType

#
# Jarfiles and their directories
#

COMPONENTS_JARFILES=benchmarkComponents
# currently the com.cosylab stuff does not compile, so we exclude it.
benchmarkComponents_DIRS=alma
benchmarkComponents_JLIBS=perftest

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
	$(RM) perftestImpl/*~  perftestImpl/*.pyc
	$(RM) *~ ../idl/*~ ../doc/*~
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"



#___oOo___
