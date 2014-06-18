#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.104 2012/01/24 01:00:04 tstaig Exp $" 
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi 2003-02-06 Create ref directory, not imported by CVS and needed for tat
# gchiozzi 2001-11-15 created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

USER_CFLAGS =
USER_LIB =

# Program to build
# Can be overwritten in the call

PROG = 

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES_L     = $(PROG) maciTestClient maciTestLogConfigClient maciTestRegistrar \
			maciContainerServicesClient maciLifeCycleClient \
			maciTestSimpleClient maciTestComponentSmartPtr \
			maciTestSimpleClientMulti maciTestSimpleClientContainerServices \
			dlopenTest

$(PROG)_OBJECTS            = $(PROG)
$(PROG)_LDFLAGS            =
$(PROG)_LIBS               = C++

maciTestSimpleClientMulti_OBJECTS			= maciTestSimpleClientMulti
maciTestSimpleClientMulti_LIBS			= maciClient
maciTestClient_OBJECTS          = maciTestClient \
                                  maciTestClientImpl
maciTestClient_LIBS             = maciClient maciTestUtils maciTestStubs

maciTestLogConfigClient_OBJECTS = maciTestLogConfigClient
maciTestLogConfigClient_LIBS    = maciClient maciTestUtils maciTestStubs

maciContainerServicesClient_OBJECTS = maciContainerServicesClient
maciContainerServicesClient_LIBS = maciClient maciTestStubs

maciLifeCycleClient_OBJECTS = maciLifeCycleClient
maciLifeCycleClient_LIBS = maciClient maciTestStubs

maciTestSimpleClient_OBJECTS =  maciTestSimpleClient
maciTestSimpleClient_LIBS = maciClient maciTestStubs

maciTestSimpleClientContainerServices_OBJECTS =  maciTestSimpleClientContainerServices
maciTestSimpleClientContainerServices_LIBS = maciClient maciTestClass maciTestStubs

maciTestComponentSmartPtr_OBJECTS =  maciTestComponentSmartPtr
maciTestComponentSmartPtr_LIBS = maciClient cppunit maciTestStubs

maciTestRegistrar_OBJECTS 	= maciTestRegistrar

dlopenTest_OBJECTS = dlopenTest
dlopenTest_LIBS = C++ dl

#
# Includes (.h) files (public and local)
# ---------------------------------
INCLUDES        = 
INCLUDES_L      = maciTestClassImpl.h maciTestLogConfigImpl.h maciHeartbeatController.h maciBlockingComponentListener.h maciContainerServicesTestClassImpl.h 

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       = maciTestClass maciTestLogConfig maciBlockingComponentListener maciTestUtils maciContainerServicesTestClass \
				  maciDynComp 
LIBRARIES_L     = 

maciBlockingComponentListener_OBJECTS = maciBlockingComponentListener
maciBlockingComponentListener_LIBS = acsComponentListener
maciTestClass_OBJECTS	= maciTestClassImpl
maciTestClass_LIBS = maciTestStubs acscomponent ACSErrTypeCORBA maciErrType
maciTestUtils_OBJECTS	= maciTestUtils
maciTestUtils_LIBS = archiveevents TAO_PortableServer
maciTestLogConfig_OBJECTS = maciTestLogConfigImpl
maciTestLogConfig_LDFLAGS= -ggdb
maciTestLogConfig_LIBS = maciTestStubs acscomponent archiveevents
maciContainerServicesTestClass_OBJECTS = maciContainerServicesTestClassImpl
maciContainerServicesTestClass_LIBS = maciTestStubs maci acsContainerServices maciBlockingComponentListener 
maciDynComp_OBJECTS = maciDynCompImpl
maciDynComp_LIBS = maciTestStubs acscomponent archiveevents

#
# Scripts (public and local)
# ----------------------------
SCRIPTS_L         = startMaciTest.sh startMaciTestClient.sh startContainerWS.sh stopManagerWS.sh maciStartStopWrongCDB.sh maciTestLogConfig.sh

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     = maciTest
TCL_SCRIPTS_L   =

maciTest_OBJECTS = maciTest
maciTest_TCLSH   = expect

#
# TCL libraries (public and local)
# ------------------------------
TCL_LIBRARIES   =
TCL_LIBRARIES_L =

#
# <brief description of tttlll library>
tttlll_OBJECTS  = 

#
# On-Line Database Files
# ----------------------

#
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
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

#
# IDL FILES
#
IDL_FILES = maciTest
maciTestStubs_LIBS = maciStubs acscomponentStubs

#
#>>>>> END OF standard rules

#
# CHOSE PLATFORM
# --------------
# Default is UNIX, for VxVorks application next line MUST BE UNCOMMENTED
#MAKE_VXWORKS = on   

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
	@mkdir -p ref
	@if [ $(WIND_BASE) ]; then \
	  cp -f TestList_BOTH.lite TestList.lite; \
	  cp -f ref_BOTH/*.ref ref/ ;\
	else\
	  cp -f TestList_WS.lite TestList.lite; \
	  cp -f ref_WS/*.ref ref/ ;\
	fi;
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) iors.dat *~ ../include/*~ ../idl/*~ ../*~ core
	$(RM) TestList.lite ref
	$(RM) tmp tatlogs
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#___oOo___
