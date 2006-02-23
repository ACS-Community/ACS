#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: Makefile,v 1.11 2005/01/31 19:46:14 dfugate Exp $" 
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# msekoran 2003-10-03 new abeans generator
# almamgr 2003-06-02 Create doc directory if does not exist
# gchiozzi 2003-05-25 Renamed in acsabeans.sh removing .sh
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# Stores also sources in jar files.
#
#DEBUG=on

#
# Jarfiles and their directories
#
JARFILES = acsabeansExamples
acsabeansExamples_DIRS = alma

USER_JFLAGS = -source 1.4 

#
# Scripts (public and local)
# ----------------------------
SCRIPTS_L         = acsabeans

#####-> #
#####-> # IDL FILES
#####-> #
#####-> IDL_FILES = 	acsexmplPowerSupply \
#####-> 		acsexmplMount \
#####-> 		acsexmplRampedPowerSupply \
#####-> 		acsexmplLamp \
#####-> 		acsexmplFridge \
#####-> 		acsexmplDoor \
#####-> 		acsexmplBuilding \
#####-> 		acsexmplAmsSeq \
#####-> 		acsexmplCalendar \
#####-> 		acsexmplHelloWorld
#####-> USER_IDL = 

#
#>>>>> END OF standard rules

#
# CHOSE PLATFORM
# --------------
# Default is UNIX, for VxVorks application next line MUST BE UNCOMMENTED
#MAKE_VXWORKS = on   

INSTALL_FILES = ../lib/*Abeans.jar

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
all:	generate do_all
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) *~ ../include/*~ ../dbl/*~ ../idl/*~ ../*~ ../../*~ ../doc/html/* core
	$(RM) ../lib/*Abeans.jar
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

generate :
	@./acsabeans

#___oOo___
