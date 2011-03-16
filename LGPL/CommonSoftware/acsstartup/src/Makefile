#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.55 2011/03/16 13:38:14 hsommer Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# mschilli 2004-10-04 added acsStartLight
# mschilli 2004-04-16 removed acsSetAndRun, added acsList
# mschilli 2003-10-28 removed everything related to java (has moved to acscommandcenter)
# mschilli 2003-10-27 added 5 install_files
# mschilli 2003-10-23 added acsSetBaseAndRun script
# dave 2003-09-04 added startACS script
# dave  2003-09-04  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

USER_LIB = -lacsutil -lACE -lTAO -lTAO_PortableServer

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = \
		acsStartContainer \
		acsStopContainer \
		acsStartORBSRVC acsStopORBSRVC \
		acsStartManager acsStopManager \
		acsStart acsStop \
		acsstartupAcsPorts acsstartupPids \
		acsStartJava \
		acsList \
		acsKillProc \
		irbrowser nsbrowser \
		acsStartLight \
		acsstartupConstants \
		acsstartupAcsInstance \
		acsstartupLogging.sh \
		acsstartupReport \
		acsStatus acsstartupLoadIFR \
		acsstartupProcesses \
		acsACSLogService \
		acsConfigurationDatabase \
		acsInterfaceRepository \
		acsLoggingService \
		acsManager \
		acsNamingService \
		acsNotifyService \
		acsAlarmService \
		acsStartContainerWithCASA \
		acsstartupNotifyPortViaErrorCode \
		acsLogProcessMem

SCRIPTS_L       =

#
# Python stuff (public and local)
# ----------------------------
PY_SCRIPTS         = killACS acsstartupContainerPort acsstartupNotifyPort \
		     acsstartupRemovePID \
		     acsContainersStatus acsNotifysStatus \
		     acsstartupCreateChannel \
                     acsdataClean
PY_SCRIPTS_L       =

PY_MODULES         =
PY_MODULES_L       =

PY_PACKAGES        =
PY_PACKAGES_L      =
pppppp_MODULES	   =

EXECUTABLES = acsstartupIrFeed
MAKE_NOIFR_CHECK = on # jagonzal: there is a cyclic dependency between acsstartupIrFeed and acsstartupLoadIFR (checker)


acsstartupIrFeed_OBJECTS = acsstartupIrFeed
acsstartupIrFeed_LIBS    = acserrStubs

# 
# IDL Files and flags
# 
IDL_FILES = ACSIRSentinel
TAO_IDLFLAGS =
USER_IDL =

#
# Jarfiles and their directories
#
JARFILES=
acsstartup_DIRS=
acsstartup_EXTRAS= 

#
# java sources in Jarfile on/off
DEBUG= on


#
# other files to be installed
#----------------------------
INSTALL_FILES =

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

#
# man pages to be done
# --------------------
MANSECTIONS = 1
MAN1 = 



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
	$(RM) *~
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"




#___oOo___
