#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.5 2006/04/20 09:02:07 gchiozzi Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# sharring  03-03-2005 created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# Version of tools.
#
JCOMMON_VER = 1.0.0-rc1
JFREECHART_VER = 1.0.0-rc1

JUNIT_JAR = ../lib/junit.jar
SERVLET_JAR = ../lib/servlet.jar
GNU_JAXP_JAR = ../lib/gnujaxp.jar
JCOMMON_JAR = ../lib/jcommon-$(JCOMMON_VER).jar

#
# Scripts (public and local)
# ----------------------------
SCRIPTS_L       = junit-jcommon junit-jfreechart

#
# Jarfiles and their directories
#
JARFILES_L=  jfreejunit
jfreejunit_DIRS=org

#
# java sources in Jarfile on/off
DEBUG=

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
all:	unpack do_all
	@echo " . . . 'all' done" 

clean : clean_all
	@rm -rf unpacked
	@rm -rf org
	@rm -rf .purify
	@rm -rf $(JUNIT_JAR)
	@rm -rf $(SERVLET_JAR)
	@rm -rf $(GNU_JAXP_JAR)
	@rm -rf $(JCOMMON_JAR)
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean_dist done"

install : install_all
	@echo " . . . installation done"

# unpack target:
# first, untar the jcommon jar 
# then, untar the jfreechart jar 
# Moves the junit source files in a standard
# ACS Java module's directory structure
unpack : unpacked
unpacked:
	@echo ". . . unpacking jcommon junit sources . . . "
	@gtar -xvzf ../src/jcommon-$(JCOMMON_VER).tar.gz `gtar -tzf../src/jcommon-$(JCOMMON_VER).tar.gz | grep "source/..*/junit/..*"`
	@cp -r jcommon-$(JCOMMON_VER)/source/org .
	@rm -rf jcommon-$(JCOMMON_VER)
	@echo ". . . unpacking jfreechart junit sources . . . "
	@gtar -xvzf ../src/jfreechart-$(JFREECHART_VER).tar.gz `gtar -tzf../src/jfreechart-$(JFREECHART_VER).tar.gz | grep "source/..*/junit/..*"`
	@gtar -xvzf ../src/jfreechart-$(JFREECHART_VER).tar.gz `gtar -tzf../src/jfreechart-$(JFREECHART_VER).tar.gz | grep "lib/*.jar"`
	@cp -r jfreechart-$(JFREECHART_VER)/source/org .
	@cp -r jfreechart-$(JFREECHART_VER)/lib/* ../lib
	@rm -rf jfreechart-$(JFREECHART_VER)
	@touch unpacked

#___oOo___
