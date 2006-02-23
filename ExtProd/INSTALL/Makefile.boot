#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile.boot,v 1.1.1.1 2003/07/14 14:30:43 psivera Exp $"
#
# Makefile of vltsw VLT Common Software build scripts
#
# who       when      what
# --------  --------  ----------------------------------------------
# psivera   02/12/99  created
# psivera   08/12/99  added buildJava
# psivera   08/12/99  added buildLSF, OSB
# psivera   19/02/01  added echo script (for Linux)
# psivera   25/02/02  added buildVLTI
# counnas   23/08/02  added buildTestIRD
# counnas   28/01/03  added buildTestIRD
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    this is a special module. The files are the 
#    installation scripts
#------------------------------------------------------------------------

AT = @
RM = rm -rf

#
# MODULE CODE DESCRIPTION:
# ------------------------

#FILE_LIST   = \
#            build                		\
#            buildAnt             		\
#            buildCCS             		\
#            buildCCS++           		\
#            buildCCSinclude      		\
#            buildClean           		\
#            buildCombat          		\
#            buildDriversEi       		\
#            buildFromArchive     		\
#            buildGNU             		\
#            buildHOS             		\
#            buildJacORB          		\
#            buildJava            		\
#            buildKit             		\
#            buildLCC             		\
#            buildOmniORB         		\
#            buildOrbacus         		\
#            buildPanel           		\
#            buildPython          		\
#            buildQserver         		\
#            buildSlx             		\
#            buildTAO             		\
#            TAO_PATCHES/TAOfix.diff	 	\
#            TAO_PATCHES/rules.local.GNU		\
#            TAO_PATCHES/wrapper_macros.GNU	\
#            TAO_PATCHES/config.h		\
#            TAO_PATCHES/platform_macros.GNU	\		
#            buildTcltk           		\
#            buildTools           		\
#            buildVLTROOT         		\
#            echo                 		\
#            standardEpilogue     		\
#            standardPrologue     
FILE_LIST   = \
		buildGNU			\
		build				\
		buildAnt			\
		buildJacORB			\
		buildJava			\
		buildOmniORB			\
		buildPython			\
		buildTAO			\
		buildTools			\
		buildCheckFileExist		\
		TAO_PATCHES			\
		CCSLite_PATCHES			\
		buildTcltk			\
		echo				\
		standardEpilogue		\
		standardPrologue
		

SOURCE_LIST   = 					\
            SOURCES/Python-2.2.tar.gz                   \
            SOURCES/gnu.tar.gz                		\
            SOURCES/gcc-SUN                		\
            SOURCES/patch.tar.gz		    	\
            SOURCES/omniNotify11b1.tar.gz               \
            SOURCES/omniORB-4.0.0.tar.gz                \
            SOURCES/omniORBpy-2.0.tar.gz                \
            SOURCES/tcltk.tar.gz                	\
            SOURCES/JacORB_1_4_1-full.tar.gz            \
            SOURCES/JacORB-ACS-2003-04-10.patch         \
            SOURCES/jakarta.tar.gz			\
            SOURCES/java				\
            SOURCES/ACE-5.3+TAO-1.3.tar.gz

#
# This Makefile must be executed always as
#
#     make REPOSITORY=<dir>
#
#
# typically: 
#     make REPOSITORY=~almamgr/REPOSITORY
#

#
# this file is just a tag to keep trace of the vltsw module 
# that has been used to build VLTROOT
#
INSTALL_FILES = ../config/vltsw.version
#
#>>>>> END OF standard rules

#
# CHOSE PLATFORM
# --------------
# Default is UNIX, for VxVorks application next line MUST BE UNCOMMENTED
#MAKE_VXWORKS = on   

#
# INCLUDE STANDARDS
# -----------------
#MAKEDIR = $(VLTROOT)/include
#include $(MAKEDIR)/vltMakefile

#
# TARGETS
# -------
prepare_installation:
	@echo "\n== copying build scripts to $(REPOSITORY)/INSTALL directory"
	$(AT) if [ -d $(REPOSITORY) ];\
	           then $(RM)  $(REPOSITORY); fi
	$(AT) mkdir            $(REPOSITORY)
	$(AT) mkdir            $(REPOSITORY)/INSTALL
	$(AT) mkdir            $(REPOSITORY)/PRODUCTS
	$(AT) cp -r $(FILE_LIST)  $(REPOSITORY)/INSTALL
	$(AT) chmod +x         $(REPOSITORY)/INSTALL/*
	@echo " . . . done"
	@echo "\n== copying sources to $(REPOSITORY)/PRODUCTS directory"
	$(AT) cp -r $(SOURCE_LIST)  $(REPOSITORY)/PRODUCTS

	@echo " . . . done"

#
#
#___oOo___
