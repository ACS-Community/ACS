#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.133 2012/03/20 14:20:41 bjeram Exp $" 
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# almamgr 2003-08-29 Removed dbl handling
# rcirami  2003-08-28 Added maciACSComponentDefines.h and maciPropertyDefines.h
# msekoran 2003-05-22 Added maciORBTask
# gchiozzi 2003-04-02 Removed nslist and nsadd. Use standard TAO ones.
# jibsen   2002-03-14 maciActivatorImpl.h made public
# gchiozzi 2001-09-20 Temoprarily added nsadd and nslist. To be removed later
# gchiozzi 2001-07-17 Removed IDL generated files from INCLUDE. Redundant.
# msekoran 2000-08-19 created
# msekoran 2001-02-19 file naming changed (adding module name prefix)
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
EXECUTABLES     = $(PROG) maciContainer maciManagerShutdown maciContainerShutdown maciReleaseComponent maciContainerLogLevel maciComponentRefDump
EXECUTABLES_L   = 

maciContainer_OBJECTS   = maciContainer 
maciContainer_LIBS = maci acsContainerServices acsComponentListener TAO_CosNotification_MC_Ext TAO_DynamicInterface logging \
		TAO_PortableServer TAO ACE baselogging acsutil loki

maciManagerShutdown_OBJECTS = maciManagerShutdown
maciManagerShutdown_LIBS = maci maciStubs ACE TAO TAO_PortableServer baselogging acsutil loki logging baselogging

maciContainerShutdown_OBJECTS = maciContainerShutdown
maciContainerShutdown_LIBS = maci maciStubs ACE TAO TAO_PortableServer loki logging baselogging 

maciReleaseComponent_OBJECTS = maciReleaseComponent
maciReleaseComponent_LIBS = maci maciStubs ACE TAO TAO_PortableServer loki acsutil logging baselogging

maciContainerLogLevel_OBJECTS = maciContainerLogLevel
maciContainerLogLevel_LIBS = maci maciStubs ACE TAO TAO_PortableServer loki logging baselogging

maciComponentRefDump_OBJECTS = maciComponentRefDump
maciComponentRefDump_LIBS= maciClient maciStubs ACSErrTypeCommon ACE TAO TAO_PortableServer loki logging baselogging \
			acserr acserrStubs

# <brief description of xxxxx program>

$(PROG)_OBJECTS            = $(PROG)
$(PROG)_LDFLAGS            =
$(PROG)_LIBS               = $(TESTLIBS) C++



#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

#
# Includes (.h) files (public and local)
# ---------------------------------
INCLUDES	= maciHelper.h maciSimpleClient.h \
              maciSimpleClientThreadHook.h maciExport.h maciContainerServices.h \
              maciContainerImpl.h maciContainerThreadHook.h \
		      maciACSComponentDefines.h maciPropertyDefines.h \
              maciLibraryManager.h maciRegistrar.h maciComponentStateManager.h\
		      maciRegistrar.i maciClientExport.h maciLogThrottleAlarmImpl.h
INCLUDES_L	= maciContainer.h maciServantManager.h \
		      maciORBTask.h 

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       = maci maciClient
LIBRARIES_L     =

maci_OBJECTS   = maciLibraryManager maciORBTask  maciContainerThreadHook \
		 maciServantManager maciContainerServices maciContainerImpl \
		 maciHelper maciComponentStateManager maciLogThrottleAlarmImpl
maci_LIBS = acsContainerServices maciErrType acsErrTypeContainerServices maciStubs acsAlSysSource acsErrTypeAlarmSourceFactory AcsContainerLogLTS alarmSource acscomponent acsQoS archiveevents
maci_LDFLAGS = -ggdb
maci_CFLAGS = -DMACI_BUILD_DLL

maciClient_OBJECTS =  maciSimpleClient maciSimpleClientThreadHook
maciClient_LIBS    =  baci maci maciErrType
maciClient_CFLAGS = -DMACICLIENT_BUILD_DLL


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
# On-Line Database Files
# ----------------------
CDB_SCHEMAS = 

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
# Java: add debug info and sources
#
DEBUG=on

#
# IDL FILES
#
IDL_FILES =

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
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) *~ ../include/*~ ../idl/*~ ../*~ ../../*~ core
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#___oOo___
