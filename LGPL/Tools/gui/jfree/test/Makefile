#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.8 2007/08/21 21:32:24 hsommer Exp $"
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
JCOMMON_VER = 1.0.15
JFREECHART_VER = 1.0.12

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
DEBUG=on

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
	@gtar -xvzf ../src/jcommon-$(JCOMMON_VER).tar.gz `gtar -tzf../src/jcommon-$(JCOMMON_VER).tar.gz | grep "source/.*/junit/..*"`
	@cp -r jcommon-$(JCOMMON_VER)/source/org .
	@rm -rf jcommon-$(JCOMMON_VER)
	@echo ". . . unpacking jfreechart junit sources . . . "
	@gtar -xvzf ../src/jfreechart-$(JFREECHART_VER).tar.gz `gtar -tzf../src/jfreechart-$(JFREECHART_VER).tar.gz | grep "tests/.*/junit/..*"`
	@cp -r jfreechart-$(JFREECHART_VER)/tests/org .
	@rm -rf jfreechart-$(JFREECHART_VER)
	@touch unpacked

#___oOo___

