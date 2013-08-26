#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id: Makefile,v 1.8 2007/08/21 11:56:38 hsommer Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# bpanta  10/12/06  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------
JCHART2D_VER = 3.2.0


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

all:	do_all clean unpack jchart2d_jar
	@echo " . . . 'all' done" 

clean : clean_all
	@rm -rf unpacked
	@rm -rf jchart2d/
	@rm -rf .purifydir
	@rm -f ../lib/jchart2d-$(JCHART2D_VER).jar
	@echo " . . . clean done"
	
clean_dist : clean clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install :
	@cp ../lib/jchart2d-$(JCHART2D_VER).jar $(INSTALL_ROOT)/lib
	@echo " . . . installation done"
	
# unpack target:
# untar(unzip) the JCHART2D jar 
unpack : unpacked
unpacked:
	@echo ". . . unpacking the tar/zip files . . . "
	mkdir jchart2d-$(JCHART2D_VER)/
	@unzip jchart2d-eclipse-project-$(JCHART2D_VER).zip -d jchart2d-$(JCHART2D_VER)
	
#modified to accomodate junit tests 2011/02/10
#pull out the jchart2d directory from inside jchart2d-$(JCHART2D_VER)/
#delete the jchart2d-$(JCHART2D_VER)/

	@cp -r jchart2d-$(JCHART2D_VER)/jchart2d .
	@rm -fr jchart2d-$(JCHART2D_VER)/
	
	@touch unpacked

# build jchart2d.jar file
jchart2d_jar: 

	@echo ". . . building jchart2d . . . "
	JDK_VERSION=$(shell java -version 2>&1 | grep 'java version' | cut -d\" -f2 | cut -d. -f1,2)

	@mkdir jchart2d/bin
	@mkdir jchart2d/test


	@#patch by Bhola Panta@NAOJ 2010/12/07
	@patch jchart2d/build.xml < include_src.patch

	@cd jchart2d; ant -Dbuild.target=$(JDK_VERSION) 
	@cp jchart2d/jchart2d-$(JCHART2D_VER).jar ../lib

	@echo "done"

#___oOo___
