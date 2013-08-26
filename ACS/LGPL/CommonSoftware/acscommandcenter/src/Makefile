#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.13 2003/10/28 10:39:44 mschilli Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# mschilli 2004-05-05 changed jManagerConfig.jar to JavaOnlyAcsConfig.jar
# mschilli 2004-04-08 added new tool configuration files
# mschilli 2003-10-28 moved java portion of acsstartup module to this module
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

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = acscommandcenter accStarter accStopper accEnableVars accStarter2
SCRIPTS_L       =

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
# IDL Files and flags
# 
IDL_FILES =
IDL_TAO_FLAGS =
USER_IDL =

#
# XML schema files and includes
#
XSDBIND = AcsCommandCenterEntities
XSDBIND_INCLUDE = systementities

#
# ACS XmlIdl generation on/off
#
XML_IDL = "AcsCommandCenterProject=alma.entity.xmlbinding.acscommandcenterproject.AcsCommandCenterProject;AcsCommandCenterTools=alma.entity.xmlbinding.acscommandcentertools.AcsCommandCenterTools"

#
# Jarfiles and their directories
#
JARFILES= acscommandcenter
acscommandcenter_DIRS= alma
acscommandcenter_EXTRAS= alma/acs/commandcenter/resources/*.gif \
                         alma/acs/commandcenter/resources/*.jpg \
                         AcsCommandCenterTools.xml \
                         AcsCommandCenterBuiltinTools.xml \
                         VERSION
acscommandcenter_JLIBS=AcsCommandCenterEntities
                         
#
# java sources in Jarfile on/off
DEBUG= on


#
# other files to be installed
#----------------------------
INSTALL_FILES = ../lib/JavaOnlyAcsConfig.jar \
					 ../lib/trilead-ssh2-build213.jar \
					 ../lib/AcsCommandCenterHelp.jar \
					 ../lib/xtmodcoll-3.0.jar


#
# Java Component Helper Classes generation on/off
#
#COMPONENT_HELPERS=


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
