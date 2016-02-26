
#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id$"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# almadev  12/02/16  created
#

# ALMA - Atacama Large Millimeter Array
# Copyright (c) ESO - European Southern Observatory, 2014
# (in the framework of the ALMA collaboration).
# All rights reserved.
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#*******************************************************************************

#*******************************************************************************
# This Makefile follows ALMA/ACS Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------
ifdef NDDSHOME

ARCH = $(shell uname -m)
GCC_VERSION_MAJOR = $(shell echo __GNUC__ | gcc -E -xc - | tail -n 1)
ifneq ($(GCC_VERSION_MAJOR),4)
        echo "ERROR: not supported gcc version for RTI DDS: $(GCC_VERSION_MAJOR)"
endif
GCC_VERSION_MINOR = $(shell echo __GNUC_MINOR__ | gcc -E -xc - | tail -n 1)
ifeq ($(GCC_VERSION_MINOR),1)
        RTIDDS_GCC_VER=4.1.2
else
        RTIDDS_GCC_VER=4.4.5
endif



# user definable C-compilation flags
USER_CFLAGS = -DRTI_UNIX -DRTI_LINUX 

#
# additional include and library search paths
USER_INC = -I$(NDDSHOME)/include -I$(NDDSHOME)/include/ndds
ifeq ($(ARCH),x86_64)
	USER_LIB = -L$(NDDSHOME)/lib/x64Linux2.6gcc$(RTIDDS_GCC_VER)
else
	USER_LIB = -L$(NDDSHOME)/lib/i86Linux2.6gcc$(RTIDDS_GCC_VER)
endif

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = bdntSenderSimulator bdntReceiverSimulator
EXECUTABLES_L   = 

bdntSenderSimulator_OBJECTS = bdntSenderSimulator BDNTSenderSimulatorFlow SenderFlowSimCallback
bdntSenderSimulator_LIBS= bulkDataNTSender

bdntReceiverSimulator_OBJECTS = bdntReceiverSimulator ReceiverFlowSimCallback
bdntReceiverSimulator_LIBS= bulkDataNTReceiver 

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
INCLUDES        =

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       =
LIBRARIES_L     =

#
# <brief description of lllll library>
lllll_OBJECTS   =

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

endif #ifdef NDDSHOME

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
# For expressing dependencies between jarfiles (parallel builds)
jjj_JLIBS= 
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
	
all: do_all
	@echo " . . . 'all' done" 

clean : clean_all
	@echo " . . . clean done"

install : install_all
	@echo " . . . installation done"
	
else

.DEFAULT_GOAL := all	
all: do_all
	@echo -e "\e[0;31mWARNING: NDDSHOME is not defined, and thus only code common to old and new BD will be build (ACSERR and IDL) for target all!!!!\e[0m" 

clean : clean_all
	@echo " . . . clean done"

install : install_all
	@echo -e "\e[0;31mWARNING: NDDSHOME is not defined, and thus only code common to old and new BD will be installed (ACSERR and IDL) for target install!!!!\e[0m"
	
endif
clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"




#___oOo___
