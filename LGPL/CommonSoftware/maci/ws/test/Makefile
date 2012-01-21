#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.103 2012/01/21 22:48:11 tstaig Exp $" 
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
			maciTestSimpleClientMulti maciTestSimpleClientContainerServices 

$(PROG)_OBJECTS            = $(PROG)
$(PROG)_LDFLAGS            =
$(PROG)_LIBS               = C++

maciTestSimpleClientMulti_OBJECTS			= maciTestSimpleClientMulti
maciTestSimpleClientMulti_LIBS			= maciClient
maciTestClient_OBJECTS          = maciTestClient \
                                  maciTestC \
                                  maciTestClientImpl
maciTestClient_LIBS             = maciClient maciTestUtils

maciTestLogConfigClient_OBJECTS = maciTestLogConfigClient maciTestC
maciTestLogConfigClient_LIBS    = maciClient maciTestUtils

maciContainerServicesClient_OBJECTS = maciContainerServicesClient \
									  maciTestC
maciContainerServicesClient_LIBS = maciClient

maciLifeCycleClient_OBJECTS = maciLifeCycleClient maciTestC
maciLifeCycleClient_LIBS = maciClient

maciTestSimpleClient_OBJECTS =  maciTestSimpleClient maciTestC
maciTestSimpleClient_LIBS = maciClient

maciTestSimpleClientContainerServices_OBJECTS =  maciTestSimpleClientContainerServices maciTestC
maciTestSimpleClientContainerServices_LIBS = maciClient maciTestClass

maciTestComponentSmartPtr_OBJECTS =  maciTestComponentSmartPtr maciTestC
maciTestComponentSmartPtr_LIBS = maciClient cppunit

maciTestRegistrar_OBJECTS 	= maciTestRegistrar

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
maciTestClass_OBJECTS	= maciTestClassImpl maciTestC maciTestS
maciTestUtils_OBJECTS	= maciTestUtils
maciTestLogConfig_OBJECTS = maciTestLogConfigImpl maciTestC maciTestS
maciTestLogConfig_LDFLAGS= -ggdb
maciContainerServicesTestClass_OBJECTS = maciContainerServicesTestClassImpl maciTestS maciTestC 
maciContainerServicesTestClass_LIBS = maci acsContainerServices maciBlockingComponentListener 
maciDynComp_OBJECTS = maciDynCompImpl maciTestS maciTestC

#
# Scripts (public and local)
# ----------------------------
SCRIPTS_L         = startMaciTest.sh startMaciTestClient.sh startContainerWS.sh maciStartStopWrongCDB.sh maciTestLogConfig.sh

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
