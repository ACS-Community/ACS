#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) European Southern Observatory, 2011 
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
# "@(#) $Id: Makefile,v 1.19 2011/08/25 17:39:18 bjeram Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# bjeram  2011-04-19  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

ARCH = $(shell uname -m)

# user definable C-compilation flags
USER_CFLAGS = -DRTI_UNIX -DRTI_LINUX 

#
# additional include and library search paths
USER_INC = -I$(NDDSHOME)/include -I$(NDDSHOME)/include/ndds
ifeq ($(ARCH),x86_64)
	USER_LIB = -L$(NDDSHOME)/lib/x64Linux2.6gcc4.1.2
else
	USER_LIB = -L$(NDDSHOME)/lib/i86Linux2.6gcc4.1.2  
endif
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
# <brief description of xxxxx program>
xxxxx_OBJECTS   =	
xxxxx_LDFLAGS   =
xxxxx_LIBS      =

#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

#
# Includes (.h) files (public only)
# ---------------------------------
INCLUDES        = bulkDataNTCallback.h bulkDataNTDDS.h bulkDataNTDDSPublisher.h bulkDataNTDDSSubscriber.h \
				bulkDataNT.h bulkDataNTPlugin.h bulkDataNTReaderListener.h bulkDataNTReceiverFlow.h \
				bulkDataNTReceiverImpl.h bulkDataNTReceiverImpl.i bulkDataNTReceiverStream.h bulkDataNTReceiverStream.i \
				bulkDataNTSenderFlow.h bulkDataNTSenderImpl.h bulkDataNTSenderStream.h bulkDataNTStream.h bulkDataNTSupport.h \
				bulkDataNTConfiguration.h bulkDataNTConfigurationParser.h


#
# Libraries (public and local)
# ----------------------------
LIBRARIES       = bulkDataNT bulkDataNTSender bulkDataNTReceiver bulkDataNTSenderImpl bulkDataNTConfigParser
LIBRARIES_L     =

#
# <brief description of lllll library>
# bulkDataNTSender_OBJECTS   = bulkDataNTDDS bulkDataNTDDSPublisher bulkDataNTBase bulkDataNTSender bulkDataNT bulkDataNTSupport bulkDataNTPlugin

bulkDataNT_OBJECTS   = bulkDataNTDDS bulkDataNTStream bulkDataNTConfiguration \
						bulkDataNT bulkDataNTSupport bulkDataNTPlugin	
#					   bulkDataNT bulkDataNTTypeSupport bulkDataNTDataWriter bulkDataNTDataReader 
bulkDataNT_LIBS = ACSErrTypeCommon ACS_BD_Errors ACS_DDS_Errors xerces-c  nddscpp nddsc nddscore
#bulkDataNT_LDFLAGS = -lnddscpp -lnddsc -lnddscore  
#-lrt -lnsl -lpthread -lstdc++ -lACE -lTAO_AV
#-ldds_cpp -ldds -lACE -lTAO_AV


#bulkDataNTSender_OBJECTS   = bulkDataNTDDSPublisher bulkDataNTSender bulkDataNTSenderImpl
bulkDataNTSender_OBJECTS    = bulkDataNTSenderStream bulkDataNTSenderFlow bulkDataNTDDSPublisher
bulkDataNTSender_LIBS = bulkDataNT bulkDataSenderStubs baci

bulkDataNTSenderImpl_OBJECTS = bulkDataNTSenderImpl
bulkDataNTSenderImpl_LIBS = bulkDataNTSender

bulkDataNTReceiver_OBJECTS   = bulkDataNTReceiverFlow bulkDataNTDDSSubscriber bulkDataNTReaderListener
bulkDataNTReceiver_LIBS = bulkDataNT

bulkDataNTConfigParser_OBJECTS = bulkDataNTConfigurationParser
bulkDataNTConfigParser_LIBS = bulkDataNTSender bulkDataNTReceiver

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         =
SCRIPTS_L       =

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     =
TCL_SCRIPTS_L   =

#
# Python stuff (public and local)
# ----------------------------
PY_SCRIPTS         =
PY_SCRIPTS_L       =

PY_MODULES         =
PY_MODULES_L       =

PY_PACKAGES        =
PY_PACKAGES_L      =
pppppp_MODULES	   =

#
# <brief description of tttttt tcl-script>
tttttt_OBJECTS  =
tttttt_TCLSH    = 
tttttt_LIBS     = 

#
# TCL libraries (public and local)
# ------------------------------
TCL_LIBRARIES   =
TCL_LIBRARIES_L =

#
# <brief description of tttlll library>
tttlll_OBJECTS  = 

#
# Configuration Database Files
# ----------------------------
CDB_SCHEMAS = 

# 
# IDL Files and flags
# 
IDL_FILES =
TAO_IDLFLAGS =
USER_IDL =
#
# Jarfiles and their directories
#
JARFILES= 
jjj_DIRS=
jjj_EXTRAS= 

ACSERRDEF = ACS_BD_Errors ACS_DDS_Errors


#
# java sources in Jarfile on/off
DEBUG= 
#
# ACS XmlIdl generation on/off
#
XML_IDL= 
#
# Java Component Helper Classes generation on/off
#
COMPONENT_HELPERS=
#
# Java Entity Classes generation on/off
#
XSDBIND=
#
# Schema Config files for the above
#
XSDBIND_INCLUDE=
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

ifdef NDDSHOME
	
all: do_dds_gen	do_all
	@echo " . . . 'all' done" 

clean : clean_all dds_clean
	@echo " . . . clean done"
else

.DEFAULT_GOAL := all	
all: 
	@echo " NDDSHOME not defined nothing to be done for target all!" 

clean : clean_all
	@echo " . . . clean done"
	
endif
clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"
	
	
do_dds_gen: bulkDataNT.cpp bulkDataNTSupport.cpp bulkDataNTPlugin.cpp bulkDataNT.h  bulkDataNTSupport.h bulkDataNTPlugin.h
	
bulkDataNT.cpp bulkDataNTSupport.cpp bulkDataNTPlugin.cpp bulkDataNT.h  bulkDataNTSupport.h bulkDataNTPlugin.h : ../idl/bulkDataNT.idl
	$(NDDSHOME)/scripts/rtiddsgen -ppDisable -namespace -replace -d ../src ../idl/bulkDataNT.idl; \
	rename .cxx .cpp *.cxx; \
	mv *.h ../include

dds_clean:
	@rm -f bulkDataNT.cpp bulkDataNTSupport.cpp bulkDataNTPlugin.cpp; \
	rm -f ../include/bulkDataNT.h  ../include/bulkDataNTSupport.h ../include/bulkDataNTPlugin.h
#___oOo___
