#*******************************************************************************
#
# "@(#) $Id: Makefile,v 1.7 2006/09/25 08:52:36 acaproni Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# almadev  15/03/05  created
#

#
# Jarfiles and their directories
#
JARFILES = laserguiplatform
laserguiplatform_DIRS = cern 
# All files that are not CVS and .java class files shall be added to the jar file.
laserguiplatform_EXTRAS = $(shell find cern -type f | grep -v CVS | grep -v ".java") resources/log4j.properties

#
# java sources in Jarfile on/off
DEBUG= 

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = laserguiStartup
SCRIPTS_L       =

#
# ASCII file to be converted into Framemaker-MIF
# --------------------
ASCII_TO_MIF = 

#
# other files to be installed
#----------------------------
INSTALL_FILES =

#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach rtos, $(RTAI_MODULES) , $($(rtos)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

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
all:	do_all manifest
	@echo " . . . 'all' done" 

clean : clean_all 
	rm manifest.touch
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

manifest : manifest.touch
manifest.touch : ../lib/laserguiplatform.jar METADATA/MANIFEST.MF
	@echo "== Adding NetBeans module manifest to jar file"
	jar uvfm ../lib/laserguiplatform.jar METADATA/MANIFEST.MF
	touch manifest.touch

laserguiextras:
	echo $(laserguiplatform_EXTRAS)
#___oOo___
