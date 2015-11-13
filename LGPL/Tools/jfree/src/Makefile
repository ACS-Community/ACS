#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id: Makefile,v 1.8 2007/08/21 11:56:38 hsommer Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# sharring  03/03/05  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    jfreechart ver 13 is available, but for now (2010/12/16) we restrict 
#    our upgrade  to ver 12 (Honglin's quicklook does not work otherwise)
#    expect to upgrade to the latest version at a later TBD date 
#------------------------------------------------------------------------
JCOMMON_VER = 1.0.15
JFREECHART_VER = 1.0.12

#
# other files to be installed
#----------------------------
INSTALL_FILES =

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
all:	do_all jcommon_jar jfreechart_jar
	@echo " . . . 'all' done" 

clean : clean_all
	@rm -rf unpacked
	@rm -rf jcommon-$(JCOMMON_VER)
	@rm -rf jfreechart-$(JFREECHART_VER)
	@rm -rf .purify
	@rm -f ../lib/jfree-jcommon-$(JCOMMON_VER).jar
	@rm -f ../lib/jfreechart-$(JFREECHART_VER).jar
	@echo " . . . clean done"
	
clean_dist : clean clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install :
	@cp ../lib/jfree-jcommon-$(JCOMMON_VER).jar $(INSTALL_ROOT)/lib
	@cp ../lib/jfreechart-$(JFREECHART_VER).jar $(INSTALL_ROOT)/lib
	@echo " . . . installation done"
	
# unpack target:
# first, untar the jcommon jar 
# then, untar the jfreechart jar 
unpack : unpacked
unpacked:
	@echo ". . . unpacking the tar files . . . "
	@gtar -xzf jcommon-$(JCOMMON_VER).tar.gz
	@gtar -xzf jfreechart-$(JFREECHART_VER).tar.gz
	@touch unpacked

# build the jcommon jar jar file.
#  Must explicitly override the ANT property "build.target" which otherwise gets set to "1.2"
#  in jcommon-1.0.10/ant/build.properties. It is used for the javac task in the build.xml file
#  and would result in a compile error under JDK 1.6.
#  An alternative would be to remove the "build.target" property by patching the ANT files, or to declare the source as 1.2.
JDK_VERSION=$(shell java -version 2>&1 | grep 'java version' | cut -d\" -f2 | cut -d. -f1,2)
jcommon_jar : unpack 
	@echo ". . . building jcommon . . . "
	@rm -rf jcommon-$(JCOMMON_VER)/lib

	@#patch by Bhola Panta@NAOJ 2010/11/30
	@patch jcommon-$(JCOMMON_VER)/ant/build.xml < java_src_jc.patch

	cd jcommon-$(JCOMMON_VER)/ant; ant -Dbuild.target=$(JDK_VERSION) compile
	@cp jcommon-$(JCOMMON_VER)/jcommon-$(JCOMMON_VER).jar ../lib/jfree-jcommon-$(JCOMMON_VER).jar


# build the jfreechart jar file
jfreechart_jar : jcommon_jar
	@echo ". . . building jfreechart . . . "

	@# Don't use any of the jar files from inside the downloaded tar.gz packages, but instead use the jars
	@# that come with ACS, or that we have built already here in this module (jfree-jcommon-$(JCOMMON_VER).jar).
	@# To be sure we don't pick up any of these pre-built jars, we simply delete the whole directory.
	@rm -rf jfreechart-$(JFREECHART_VER)/lib/*

	@# Bring back the jcommon jar file, but use the one we just built.
	@# Note that with the previous version jfreechart 1.0.6, the build.xml file took the javac classpath from the ant property "jcommon.jar",
	@# so that instead of copying and renaming the freshly produced jcommon jar file here, 
	@# it was sufficient to reference it via "ant -Djcommon.jar=../../../lib/jfree-jcommon-$(JCOMMON_VER).jar compile" in the call below.
	cp ../lib/jfree-jcommon-$(JCOMMON_VER).jar jfreechart-$(JFREECHART_VER)/lib/jcommon-$(JCOMMON_VER).jar

	# The servlet.jar file comes with jfreechart (and just got deleted in the lib dir). 
	# Since it is not part of ACS, we better not even compile the jfree classes that need the javax.servlet. packages.
	@rm -rf jfreechart-$(JFREECHART_VER)/source/org/jfree/chart/servlet

	@#patch by Bhola Panta@NAOJ 2010/11/30
	patch jfreechart-$(JFREECHART_VER)/ant/build.xml < java_src.patch

	@# Apply patch for http://jira.alma.cl/browse/CSV-182 , changing
	@# 'Domain Axis' to 'Abscissa'
	@# 'Range Axis' to 'Ordinate' 
	@# It looks like this should be done in one or both of these files:
	@#   jfreechart-1.0.13/source/org/jfree/chart/editor/LocalizationBundle.properties
	@#   jfreechart-1.0.13/source/org/jfree/chart/LocalizationBundle.properties

	@#2010/12/10 patch for /chart/LocalizationBundle.properties 
	patch jfreechart-$(JFREECHART_VER)/source/org/jfree/chart/LocalizationBundle.properties < axis_name.patch

	cd jfreechart-$(JFREECHART_VER)/ant; ant compile
	@cp jfreechart-$(JFREECHART_VER)/jfreechart-$(JFREECHART_VER).jar ../lib
	@echo "done"

#___oOo___
