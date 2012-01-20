#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id: Makefile,v 1.28 2012/01/20 22:07:43 tstaig Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# almadev  21/06/06  created
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
USER_LIB = -lTAO -lACE -lTAO_PortableServer -lacserr -lmaciErrType -lTAO_DsLogAdmin -lTAO_CosNaming -lTAO_CosProperty_Serv -lTAO_IORTable -llogging -lTAO_DynamicAny -lacsutil -lACSErrTypeCommon -lACSErrTypeCORBA -lTAO_Messaging -lacsQoS -lacsAlSysSource -lAcsAlarmSystemStubs

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = acsservicesdaemon acsservicesdaemonStop \
                  acsdaemonNamingServiceImp acsdaemonNotificationServiceImp \
                  acsdaemonInterfaceRepositoryImp acsdaemonLoggingServiceImp \
                  acsdaemonACSLogServiceImp acsdaemonConfigurationDatabaseImp \
                  acsdaemonManagerImp acsdaemonAlarmServiceImp \
                  acscontainerdaemon acsdaemonStartContainer \
                  acsdaemonStopContainer acsdaemonStartAcs \
                  acsdaemonStopAcs acsdaemonStatusAcs \
                  acscontainerdaemonSmartStart acscontainerdaemonStop \
                  acsdaemonImpStop
EXECUTABLES_L   = 

#
# <brief description of xxxxx program>
acscontainerdaemon_OBJECTS   =	acscontainerdaemon acsContainerHandlerImpl acsdaemonORBTask
acscontainerdaemon_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs

acsdaemonStartContainer_OBJECTS   =	acsdaemonStartContainer
acsdaemonStartContainer_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs


acsdaemonStopContainer_OBJECTS   =	acsdaemonStopContainer
acsdaemonStopContainer_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs

acsservicesdaemon_OBJECTS   =	acsservicesdaemon acsServicesHandlerImpl acsRequest acsServiceController acsdaemonORBTask
acsservicesdaemon_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsThread expat acsQoS maciErrType maciStubs AcsAlarmSystemStubs ACSErrTypeCORBA

acsdaemonStartAcs_OBJECTS   =	acsdaemonStartAcs
acsdaemonStartAcs_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs


acsdaemonStopAcs_OBJECTS   =	acsdaemonStopAcs
acsdaemonStopAcs_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs

acsdaemonStatusAcs_OBJECTS   =	acsdaemonStatusAcs
acsdaemonStatusAcs_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs

acscontainerdaemonStop_OBJECTS   =	acscontainerdaemonStop
acscontainerdaemonStop_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs

acscontainerdaemonSmartStart_OBJECTS   =	acscontainerdaemonSmartStart
acscontainerdaemonSmartStart_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs

acsservicesdaemonStop_OBJECTS   =	acsservicesdaemonStop
acsservicesdaemonStop_LIBS      =	acscomponentStubs logging acsdaemonErrType acsdaemonStubs maciErrType

acsdaemonNamingServiceImp_OBJECTS   =	acsdaemonNamingServiceImp acsRequest acsServiceController acsdaemonORBTask
acsdaemonNamingServiceImp_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsThread acsQoS

acsdaemonNotificationServiceImp_OBJECTS   =	acsdaemonNotificationServiceImp acsRequest acsServiceController acsdaemonORBTask
acsdaemonNotificationServiceImp_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsThread acsQoS TAO_CosNotification_Serv

acsdaemonInterfaceRepositoryImp_OBJECTS   =	acsdaemonInterfaceRepositoryImp acsRequest acsServiceController acsdaemonORBTask
acsdaemonInterfaceRepositoryImp_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsThread acsQoS

acsdaemonLoggingServiceImp_OBJECTS   =	acsdaemonLoggingServiceImp acsRequest acsServiceController acsdaemonORBTask
acsdaemonLoggingServiceImp_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsThread acsQoS

acsdaemonACSLogServiceImp_OBJECTS   =	acsdaemonACSLogServiceImp acsRequest acsServiceController acsdaemonORBTask
acsdaemonACSLogServiceImp_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsThread acsQoS

acsdaemonConfigurationDatabaseImp_OBJECTS   =	acsdaemonConfigurationDatabaseImp acsRequest acsServiceController acsdaemonORBTask
acsdaemonConfigurationDatabaseImp_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsThread acsQoS

acsdaemonManagerImp_OBJECTS   =	acsdaemonManagerImp acsRequest acsServiceController acsdaemonORBTask
acsdaemonManagerImp_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsThread acsQoS 

acsdaemonAlarmServiceImp_OBJECTS   = acsdaemonAlarmServiceImp acsRequest acsServiceController acsdaemonORBTask
acsdaemonAlarmServiceImp_LIBS      = acscomponentStubs acsdaemonErrType acsdaemonStubs acsThread acsQoS


acsdaemonImpStop_OBJECTS   =       acsdaemonImpStop
acsdaemonImpStop_LIBS      =       acscomponentStubs acsdaemonErrType acsdaemonStubs

#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

#
# Includes (.h) files (public only)
# ---------------------------------
INCLUDES        = acsDaemonImpl.h acsdaemonORBTask.h

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
SCRIPTS         = acsdaemonImpStopAll
SCRIPTS_L       =

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
CDB_SCHEMAS = 

# 
# IDL Files and flags
# 

IDL_FILES = 
IDL_TAO_FLAGS =
USER_IDL =


ACSERRDEF = 

#
# Jarfiles and their directories
#
JARFILES= 
jjj_DIRS=
jjj_EXTRAS= 
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
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"


#___oOo___
