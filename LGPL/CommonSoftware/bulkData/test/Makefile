#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.34 2011/10/14 17:58:17 bjeram Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# alma  2004-03-30  created
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

MAKE_ONLY="C++"
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
		-lm
#		-lmd5

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = bulkDataEx1 \
		  bulkDataEx2 \
		  bulkDataEx3 \
		  bulkDataSendRecvEx \
		  bulkDataDistributerEx \
		  bulkDataPerf \
		  bulkDataDistrPerf \
		  bulkDataNotificationTest \
	   	  bulkDataThreadTest

EXECUTABLES_L   = 

#
# <brief description of xxxxx program>
bulkDataEx1_OBJECTS   = bulkDataEx1	
bulkDataEx1_LIBS      = bulkDataReceiverStubs \
			bulkDataSenderStubs \
			bulkDataStubs \
			bulkDataCallback \
			ACSBulkDataError

bulkDataEx2_OBJECTS   = bulkDataEx2	
bulkDataEx2_LIBS      = bulkDataReceiverStubs \
			bulkDataReceiverEx2Stubs \
			bulkDataSenderStubs \
			bulkDataSenderEx2Stubs \
			bulkDataStubs \
			bulkDataCallback \
			ACSBulkDataError

bulkDataEx3_OBJECTS   = bulkDataEx3	
bulkDataEx3_LIBS      = bulkDataReceiverStubs \
			bulkDataReceiverEx3Stubs \
			bulkDataSenderStubs \
			bulkDataSenderEx3Stubs \
			bulkDataStubs \
			bulkDataCallback \
			ACSBulkDataError \
			ACSBulkDataStatus

bulkDataSendRecvEx_OBJECTS   = bulkDataSendRecvEx	
bulkDataSendRecvEx_LIBS      = bulkDataSendRecvStubs \
				bulkDataReceiverStubs \
				bulkDataSenderStubs \
				bulkDataStubs \
				bulkDataCallback \
				ACSBulkDataError

bulkDataDistributerEx_OBJECTS   = bulkDataDistributerEx	
bulkDataDistributerEx_LIBS      = bulkDataDistributerStubs \
					bulkDataSenderStubs \
					bulkDataSenderDistrStubs \
					bulkDataReceiverStubs \
					bulkDataReceiverDistr1Stubs \
					bulkDataReceiverDistr2Stubs \
					bulkDataStubs \
					bulkDataCallback \
					ACSBulkDataError

bulkDataPerf_OBJECTS   = bulkDataPerf	
bulkDataPerf_LIBS      = bulkDataReceiverStubs \
			bulkDataReceiver1PerfStubs \
			bulkDataSenderStubs \
			bulkDataSenderPerfStubs \
			bulkDataStubs \
			bulkDataCallback \
			ACSBulkDataError

bulkDataDistrPerf_OBJECTS   = bulkDataDistrPerf	
bulkDataDistrPerf_LIBS      = bulkDataReceiverStubs \
			      bulkDataReceiver1PerfStubs \
			      bulkDataSenderStubs \
			      bulkDataSenderPerfStubs \
			      bulkDataDistributerStubs \
			      bulkDataStubs \
			      bulkDataCallback \
			      ACSBulkDataError

bulkDataNotificationTest_OBJECTS   = bulkDataNotificationTest	
bulkDataNotificationTest_LIBS      = bulkDataReceiverStubs \
			      	     bulkDataSenderStubs \
			             bulkDataDistributerStubs \
			             bulkDataStubs \
			             bulkDataCallback \
			             ACSBulkDataError

bulkDataThreadTest_OBJECTS   = bulkDataThreadTest	
bulkDataThreadTest_LIBS      = bulkDataSenderStubs \
			       bulkDataSenderDistrStubs \
			       bulkDataDistributerStubs \
			       bulkDataReceiverStubs \
			       bulkDataStubs \
			       bulkDataCallback \
			       ACSBulkDataError


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
LIBRARIES       = bulkDataReceiverLibEx1 \
		  bulkDataReceiverLibEx2 \
		  bulkDataReceiverLibEx3 \
		  bulkDataReceiverLibNotif \
		  bulkDataReceiver1LibNotif \
		  bulkDataSenderLibEx1 \
		  bulkDataSenderLibEx2 \
	 	  bulkDataSenderLibEx3 \
		  bulkDataSenderLibPerf \
	          bulkDataSenderLibNotif \
		  bulkDataReceiver1LibPerf \
		  bulkDataSendRecvLib \
		  bulkDataSenderDistrLib \
		  bulkDataReceiverDistr1Lib \
		  bulkDataReceiverDistr2Lib \
		  bulkDataSenderLibThread \
		  bulkDataReceiverLibThread
LIBRARIES_L     =

#
# <brief description of lllll library>

bulkDataReceiverLibEx1_OBJECTS   = bulkDataReceiverCbEx1
bulkDataReceiverLibEx1_LIBS      = bulkDataReceiverStubs bulkDataStubs bulkDataCallback ACSBulkDataError

bulkDataReceiverLibEx2_OBJECTS   = bulkDataReceiverCbEx2
bulkDataReceiverLibEx2_LIBS      = bulkDataReceiverStubs bulkDataReceiverEx2Stubs bulkDataStubs \
				   bulkDataCallback ACSBulkDataError

bulkDataReceiverLibEx3_OBJECTS   = bulkDataReceiverCbEx3
bulkDataReceiverLibEx3_LIBS      = bulkDataReceiverStubs bulkDataReceiverEx3Stubs bulkDataStubs \
				   bulkDataCallback ACSBulkDataError ACSBulkDataStatus

bulkDataReceiverLibNotif_OBJECTS   = bulkDataReceiverCbNotif
bulkDataReceiverLibNotif_LIBS      = bulkDataReceiverStubs bulkDataStubs \
				     bulkDataCallback ACSBulkDataError ACSBulkDataStatus

bulkDataReceiver1LibNotif_OBJECTS   = bulkDataReceiver1CbNotif
bulkDataReceiver1LibNotif_LIBS      = bulkDataReceiverStubs bulkDataStubs \
				      bulkDataCallback ACSBulkDataError ACSBulkDataStatus

bulkDataSenderLibEx1_OBJECTS   = bulkDataSenderEx1Impl
bulkDataSenderLibEx1_LIBS      = bulkDataSenderEx1Stubs bulkDataSenderStubs bulkDataReceiverStubs bulkDataStubs ACSBulkDataError

bulkDataSenderLibEx2_OBJECTS   = bulkDataSenderEx2Impl
bulkDataSenderLibEx2_LIBS      = bulkDataSenderEx2Stubs bulkDataSenderStubs bulkDataReceiverStubs bulkDataStubs ACSBulkDataError

bulkDataSenderLibEx3_OBJECTS   = bulkDataSenderEx3Impl
bulkDataSenderLibEx3_LIBS      = bulkDataSenderEx3Stubs bulkDataSenderStubs bulkDataReceiverStubs bulkDataStubs ACSBulkDataError

bulkDataSenderLibPerf_OBJECTS   = bulkDataSenderPerfImpl
bulkDataSenderLibPerf_LIBS      = bulkDataReceiverStubs bulkDataSenderPerfStubs bulkDataSenderStubs bulkDataStubs ACSBulkDataError

bulkDataSenderLibNotif_OBJECTS   = bulkDataSenderNotifImpl
bulkDataSenderLibNotif_LIBS      = bulkDataReceiverStubs bulkDataSenderStubs bulkDataStubs ACSBulkDataError

bulkDataReceiver1LibPerf_OBJECTS   = bulkDataReceiver1PerfCb
bulkDataReceiver1LibPerf_LIBS      = bulkDataReceiverStubs bulkDataReceiver1PerfStubs bulkDataStubs \
				     bulkDataCallback ACSBulkDataError

bulkDataSendRecvLib_OBJECTS   = bulkDataSendRecvCb
bulkDataSendRecvLib_LIBS      = bulkDataSendRecvStubs bulkDataStubs bulkDataSenderStubs bulkDataReceiverStubs \
				bulkDataCallback ACSBulkDataError

bulkDataSenderDistrLib_OBJECTS   = bulkDataSenderDistr
bulkDataSenderDistrLib_LIBS      = bulkDataReceiverStubs bulkDataSenderDistrStubs bulkDataSenderStubs \
				   bulkDataStubs ACSBulkDataError

bulkDataReceiverDistr1Lib_OBJECTS   = bulkDataReceiverCbDistr1
bulkDataReceiverDistr1Lib_LIBS      =  bulkDataReceiverStubs bulkDataReceiverDistr1Stubs bulkDataStubs \
				       bulkDataCallback ACSBulkDataError

bulkDataReceiverDistr2Lib_OBJECTS   = bulkDataReceiverDistr2Impl bulkDataReceiverCbDistr2
bulkDataReceiverDistr2Lib_LIBS      = bulkDataReceiverStubs bulkDataReceiverDistr2Stubs bulkDataStubs \
				      bulkDataCallback ACSBulkDataError

bulkDataSenderLibThread_OBJECTS   = bulkDataSenderThreadImpl
bulkDataSenderLibThread_LIBS      = bulkDataSenderDistrStubs bulkDataReceiverStubs bulkDataSenderStubs bulkDataStubs ACSBulkDataError

bulkDataReceiverLibThread_OBJECTS = bulkDataReceiverCbThread
bulkDataReceiverLibThread_LIBS    = bulkDataReceiverStubs bulkDataStubs bulkDataCallback ACSBulkDataError

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         =
SCRIPTS_L       = bulkDataTATPrologue \
		  bulkDataTATEpilogue \
		  bulkDataTATEnvironment \
		  bulkDataExportTATEnvironment \
		  bulkDataTestRun.sh \
		  bulkDataDistrTestRun.sh \
		  bulkDataNotifTestRun.sh \
		  bulkDataThreadTestRun.sh \
		  startBulkDataTest.sh \
		  startBulkDataDistrTest.sh \
		  startBulkDataNotifTest.sh \
		  startBulkDataThreadTest.sh

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     =
TCL_SCRIPTS_L   =

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
# Configuration Database Files
# ----------------------------
CDB_SCHEMAS = BulkDataSendRecv

# 
# IDL Files and flags
# 
IDL_FILES = bulkDataSenderEx1 bulkDataReceiverEx2 bulkDataSenderEx2 \
	    bulkDataSenderEx3 bulkDataReceiverEx3 bulkDataSendRecv \
	    bulkDataSenderDistr bulkDataReceiverDistr1 bulkDataReceiverDistr2 \
	    bulkDataSenderPerf bulkDataReceiver1Perf
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
	$(RM) *~ ../include/*~ ../dbl/*~ ../idl/*~ ../*~ ../../*~ core*
	$(RM) tmp
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

db : db_all
	@echo " . . . ../DB done"


#___oOo___
