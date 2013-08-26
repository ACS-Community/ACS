#*******************************************************************************
#  ALMA - Atacama Large Millimiter Array
#  (c) European Southern Observatory, 2002
#  Copyright by ESO (in the framework of the ALMA collaboration),
#  All rights reserved
#
#  This library is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public
#  License as published by the Free Software Foundation; either
#  version 2.1 of the License, or (at your option) any later version.
#
#  This library is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public
#  License along with this library; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
#  MA 02111-1307  USA
#
#
# "@(#) $Id: Makefile,v 1.12 2012/01/21 22:48:11 tstaig Exp $" 
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi 2003-02-06 Added creation of ref directory (CVS cannot extract it)
# gchiozzi 2002-12-12 Removed CCS library
# bjeram 2002-07-01 renamed baciTestThreds (TCL) to baciTestThreadsTCL (it was collision with baciTestThreads C++ program 
# gchiozzi 2001-10-19 Added baciTestUtils library
# gchiozzi 2001-04-20 Added removal of *.diff and *.rep files
# rlemke 2001-03-20 Created from mount module example
# almamgr 2000-07-20 created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

USER_CFLAGS =
USER_LIB =

# Program to build
# Can be overwritten in the call

PROG = 

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = $(PROG) acscomponentTestServer acscomponentTestClient

EXECUTABLES_L     = 

# ---> CDB_Test in text directory


#
# <brief description of xxxxx program>

acscomponentTestServer_OBJECTS = acscomponentTestServer
acscomponentTestServer_LIBS    = acscomponentTest acscomponentTestStubs acsContainerServices maciStubs

acscomponentTestClient_OBJECTS = acscomponentTestClient
acscomponentTestClient_LIBS    = acscomponentTest acscomponentTestStubs

#
# Includes (.h) files (public and local)
# ---------------------------------
INCLUDES        = 
INCLUDES_L      =

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       = acscomponentTest
LIBRARIES_L     = 

acscomponentTest_OBJECTS = acscomponentTestImpl
acscomponentTest_LIBS = acscomponentTestStubs acscomponent 

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = startAcscomponentTest.sh acscomponentTestRun.sh
SCRIPTS_L         = 

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     = acscomponentTestTcl

acscomponentTestTcl_OBJECTS = acscomponentTestTcl
acscomponentTestTcl_TCLSH   = expect

TCL_SCRIPTS_L   =

#
# TCL libraries (public and local)
# ------------------------------
TCL_LIBRARIES   =
TCL_LIBRARIES_L =

#
# <brief description of tttlll library>
tttlll_OBJECTS  = 

#
# On-Line Database Files
# ----------------------


#
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
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

#
# IDL FILES
#
IDL_FILES = acscomponentTest
acscomponentTestStubs_LIBS = acscomponentStubs

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

MAKEDIRTMP := $(shell searchFile include/acsMakefile)
ifneq ($(MAKEDIRTMP),\#error\#)
   MAKEDIR := $(MAKEDIRTMP)/include
   include $(MAKEDIR)/acsMakefile
endif


#
# TARGETS
# -------
all:	do_all
	@mkdir -p ref
	@mkdir -p tmp
	@if [ $(WIND_BASE) ]; then \
	  cp -f TestList_BOTH.lite TestList.lite; \
          cp -R ref_BOTH/* ref ;\
	else\
	   cp -f TestList_WS.lite TestList.lite; \
	   cp -R ref_WS/* ref ;\
	fi;
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) iors.dat *~ ../include/*~ ../idl/*~ ../*~ core
	$(RM) TestList.lite ref tatlogs
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#___oOo___
