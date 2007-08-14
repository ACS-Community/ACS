#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id: Makefile,v 1.7 2007/08/14 16:51:35 hsommer Exp $"
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
#    None
#------------------------------------------------------------------------
JCOMMON_VER = 1.0.10
JFREECHART_VER = 1.0.6

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
### GCH 2005-04-28
### I commented out removing uneeded jars
### Because of the way the classpath is built in the ant build.xml
### file, we have to use these jars and we cannot simply remove them.
### Some work would be needed to fix this problem
### This is bad because we do not build with the actual jars that will be
### used in the ACSROOT.
### all:	do_all remove_unneeded_jars jcommon_jar jfreechart_jar
all:	do_all jcommon_jar jfreechart_jar
	@echo " . . . 'all' done" 

clean : clean_all
	@rm -rf unpacked patched
	@rm -rf jcommon-$(JCOMMON_VER)
	@rm -rf jfreechart-$(JFREECHART_VER)
	@rm -rf .purify
	@rm -rf ../lib/jcommon-$(JCOMMON_VER).jar
	@rm -rf ../lib/jfreechart-$(JFREECHART_VER).jar
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install :
	@cp ../lib/jcommon-$(JCOMMON_VER).jar $(INSTALL_ROOT)/lib
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

patch_jfree : patched
patched:
	@patch jfreechart-$(JFREECHART_VER)/ant/build.xml jfreechart-build.xml.patch
	@touch patched

# remove unneeded jar files to avoid any possible confusion during build/installation.
remove_unneeded_jars :
	@rm -rf jcommon-$(JCOMMON_VER)/lib/junit.jar
	@rm -rf jfreechart-$(JFREECHART_VER)/jfreechart-$(JFREECHART_VER)-demo.jar
	@rm -rf jfreechart-$(JFREECHART_VER)/lib/gnujaxp.jar
	@rm -rf jfreechart-$(JFREECHART_VER)/lib/jcommon-0.9.6.jar
	@rm -rf jfreechart-$(JFREECHART_VER)/lib/junit.jar
	@rm -rf jfreechart-$(JFREECHART_VER)/lib/servlet.jar

# build the jfreechart jar file
# HSO 2005-09-20: commented out the patching because the ant target 'compile-junit-tests' seems never used anywhere.
# waiting for reply from SHA on its usage. If it's required, the patch file must be re-created, otherwise deleted.
#jfreechart_jar : unpack patch_jfree
jfreechart_jar : unpack 
	@echo ". . . building jfreechart . . . "
	@cd jfreechart-$(JFREECHART_VER)/ant; ant compile
	@cp jfreechart-$(JFREECHART_VER)/jfreechart-$(JFREECHART_VER).jar ../lib

# build the jcommon jar and the jcommon junit tests jar file
#  Must explicitly override the ANT property "build.target" which otherwise gets set to "1.2" 
#  in jcommon-1.0.0-rc1/ant/build.properties. It is used for the javac task in the build.xml file
#  and would result in a compile error under JDK 1.5. 
#  An alternative would be to remove the "build.target" property by patching the ANT files, or to declare the source as 1.2.
JDK_VERSION=$(shell java -version 2>&1 | grep 'java version' | cut -d\" -f2 | cut -d. -f1,2)
jcommon_jar : unpack 
	@echo ". . . building jcommon . . . "
	cd jcommon-$(JCOMMON_VER)/ant; ant -Dbuild.target=$(JDK_VERSION) compile
	@cp jcommon-$(JCOMMON_VER)/jcommon-$(JCOMMON_VER).jar ../lib

#___oOo___
