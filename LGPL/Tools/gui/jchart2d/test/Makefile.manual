#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.8 2007/08/21 21:32:24 hsommer Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# bpanta    05-02-2011 created
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
JCHART2D_VER = 3.2.0


JCHART2D_JUNIT_JAR = ../lib/jchart2djunit.jar

#
# Scripts (public and local)
# ----------------------------
SCRIPTS_L       = junit-jchart2d

#
# Jarfiles and their directories
#
JARFILES_L=  jchart2djunit

jchart2djunit_DIRS=info

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
	@rm -rf .purifydir
	@rm -f $(JCHART2D_JUNIT_JAR) 
	@rm -rf info
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean_dist done"

install : install_all
	@echo " . . . installation done"

# note:
# the distributed zip file containing java sources does not have junit test files
# instead, the test files have been downloaded from SourceForge CVS and 
# zipped as info.zip. We need to unzip the test files and put them
# in /info under /test directory

unpack : unpacked
unpacked:
	@echo ". . . unpacking the test(zip) files . . . "

	@unzip info.zip

	@touch unpacked

#___oOo___

