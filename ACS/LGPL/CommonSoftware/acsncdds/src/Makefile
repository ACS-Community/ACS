#*******************************************************************************
#
# "@(#) $Id: Makefile,v 1.18 2012/01/21 22:48:11 tstaig Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# javarias  01/09/08  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# user definable C-compilation flags
# USER_CFLAGS = 

#
# additional include and library search paths
USER_LIB =

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = 
EXECUTABLES_L   = 

#
# Includes (.h) files (public only)
# ---------------------------------
INCLUDES      =  DDSHelper.h DDSPublisher.h DDSSubscriber.h \
		 DataReaderListener.h acsddsncDataReaderListener.h \
		 acsddsncCDBProperties.h 

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       = acsddsnc
LIBRARIES_L     =

#
# Library that provides all the core functionality of DDS Notification Channel in ACS
acsddsnc_OBJECTS  = DDSHelperImpl DDSPublisherImpl DDSSubscriberImpl DataReaderListenerImpl acsddsncCDBPropertiesImpl
acsddsnc_LIBS	  = OpenDDS_Dcps SimpleTcp maci

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = startDCPSInfoRepo
SCRIPTS_L       =


#
# Configuration Database Files
# ----------------------------
CDB_SCHEMAS = 

# 
# DDS and IDL Files and flags
#
USE_OPENDDS = 1
DDS_FILES = 
IDL_FILES = 

# man pages to be done
# --------------------
MANSECTIONS =
MAN1 =
MAN3 =
MAN5 =
MAN7 =
MAN8 =

#
# local man pages
# ---------------
MANl =

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
all: do_all
	@echo " . . . 'all' done" 

clean : clean_all 
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"


#___oOo___
