#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id: Makefile,v 1.31 2012/05/15 09:06:34 msekoran Exp $"
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
USER_LIB =

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
acscontainerdaemon_OBJECTS   =  acscontainerdaemon acsContainerHandlerImpl acsdaemonORBTask acsRequest acsServiceController acsDaemonUtils
acscontainerdaemon_LIBS      =  acscomponentStubs acsdaemonErrType acsdaemonStubs ACSErrTypeCommon acsThread acsQoS ACSErrTypeCommon ACSErrTypeCORBA maciErrType maciStubs AcsAlarmSystemStubs \
		ACE TAO

acsdaemonStartContainer_OBJECTS   =	acsdaemonStartContainer
acsdaemonStartContainer_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsutil ACSErrTypeCommon ACSErrTypeCORBA acsQoS acsAlSysSource AcsAlarmSystemStubs \
		ACE TAO
acsdaemonStopContainer_OBJECTS   =	acsdaemonStopContainer
acsdaemonStopContainer_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsutil ACSErrTypeCommon ACSErrTypeCORBA acsQoS acsAlSysSource AcsAlarmSystemStubs \

acsservicesdaemon_OBJECTS   =	acsservicesdaemon acsServicesHandlerImpl acsRequest acsServiceController acsdaemonORBTask acsDaemonUtils
acsservicesdaemon_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsThread expat acsQoS maciErrType maciStubs AcsAlarmSystemStubs ACSErrTypeCORBA \
		ACE

acsdaemonStartAcs_OBJECTS   =	acsdaemonStartAcs
acsdaemonStartAcs_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsutil ACSErrTypeCommon ACSErrTypeCORBA acsQoS acsAlSysSource AcsAlarmSystemStubs


acsdaemonStopAcs_OBJECTS   =	acsdaemonStopAcs
acsdaemonStopAcs_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsutil ACSErrTypeCommon ACSErrTypeCORBA acsQoS acsAlSysSource AcsAlarmSystemStubs

acsdaemonStatusAcs_OBJECTS   =	acsdaemonStatusAcs
acsdaemonStatusAcs_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsutil ACSErrTypeCommon ACSErrTypeCORBA acsQoS acsAlSysSource AcsAlarmSystemStubs

acscontainerdaemonStop_OBJECTS   =	acscontainerdaemonStop
acscontainerdaemonStop_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsutil ACSErrTypeCommon ACSErrTypeCORBA acsQoS acsAlSysSource AcsAlarmSystemStubs

acscontainerdaemonSmartStart_OBJECTS   =	acscontainerdaemonSmartStart
acscontainerdaemonSmartStart_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsutil ACSErrTypeCommon ACSErrTypeCORBA acsQoS acsAlSysSource AcsAlarmSystemStubs

acsservicesdaemonStop_OBJECTS   =	acsservicesdaemonStop
acsservicesdaemonStop_LIBS      =	acscomponentStubs logging acsdaemonErrType acsdaemonStubs maciErrType acsutil ACSErrTypeCommon ACSErrTypeCORBA acsQoS acsAlSysSource AcsAlarmSystemStubs

acsdaemonNamingServiceImp_OBJECTS   =	acsdaemonNamingServiceImp acsRequest acsServiceController acsdaemonORBTask acsDaemonUtils
acsdaemonNamingServiceImp_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsThread acsutil ACSErrTypeCommon ACSErrTypeCORBA acsQoS acsAlSysSource AcsAlarmSystemStubs \
		ACE

acsdaemonNotificationServiceImp_OBJECTS   =	acsdaemonNotificationServiceImp acsRequest acsServiceController acsdaemonORBTask acsDaemonUtils
acsdaemonNotificationServiceImp_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsThread acsutil ACSErrTypeCommon ACSErrTypeCORBA acsQoS acsAlSysSource AcsAlarmSystemStubs TAO_CosNotification_Serv \
		ACE

acsdaemonInterfaceRepositoryImp_OBJECTS   =	acsdaemonInterfaceRepositoryImp acsRequest acsServiceController acsdaemonORBTask acsDaemonUtils
acsdaemonInterfaceRepositoryImp_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsThread acsutil ACSErrTypeCommon ACSErrTypeCORBA acsQoS acsAlSysSource AcsAlarmSystemStubs \
		ACE

acsdaemonLoggingServiceImp_OBJECTS   =	acsdaemonLoggingServiceImp acsRequest acsServiceController acsdaemonORBTask acsDaemonUtils
acsdaemonLoggingServiceImp_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsThread acsutil ACSErrTypeCommon ACSErrTypeCORBA acsQoS acsAlSysSource AcsAlarmSystemStubs \
		ACE

acsdaemonACSLogServiceImp_OBJECTS   =	acsdaemonACSLogServiceImp acsRequest acsServiceController acsdaemonORBTask acsDaemonUtils
acsdaemonACSLogServiceImp_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsThread acsutil ACSErrTypeCommon ACSErrTypeCORBA acsQoS acsAlSysSource AcsAlarmSystemStubs \
		ACE

acsdaemonConfigurationDatabaseImp_OBJECTS   =	acsdaemonConfigurationDatabaseImp acsRequest acsServiceController acsdaemonORBTask acsDaemonUtils
acsdaemonConfigurationDatabaseImp_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsThread acsutil ACSErrTypeCommon ACSErrTypeCORBA acsQoS acsAlSysSource AcsAlarmSystemStubs

acsdaemonManagerImp_OBJECTS   =	acsdaemonManagerImp acsRequest acsServiceController acsdaemonORBTask acsDaemonUtils
acsdaemonManagerImp_LIBS      =	acscomponentStubs acsdaemonErrType acsdaemonStubs acsThread acsutil ACSErrTypeCommon ACSErrTypeCORBA acsQoS acsAlSysSource AcsAlarmSystemStubs

acsdaemonAlarmServiceImp_OBJECTS   = acsdaemonAlarmServiceImp acsRequest acsServiceController acsdaemonORBTask acsDaemonUtils
acsdaemonAlarmServiceImp_LIBS      = acscomponentStubs acsdaemonErrType acsdaemonStubs acsThread acsutil ACSErrTypeCommon ACSErrTypeCORBA acsQoS acsAlSysSource AcsAlarmSystemStubs


acsdaemonImpStop_OBJECTS   =       acsdaemonImpStop
acsdaemonImpStop_LIBS      =       acscomponentStubs acsdaemonErrType acsdaemonStubs acsutil ACSErrTypeCommon ACSErrTypeCORBA acsQoS acsAlSysSource AcsAlarmSystemStubs

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
