#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) European Southern Observatory, 2002
# Copyright by ESO (in the framework of the ALMA collaboration)
# and Cosylab 2002, All rights reserved
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307  USA
#
# "@(#) $Id: Makefile,v 1.70 2012/01/20 22:07:43 tstaig Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi 2003-02-06 Added creation of ref directory (CVS cannot extract it)
# bjeram 2002-06-03 added addDataTest
# bjeram 2002-01-21 remove -lmaci
# gchiozzi 2002-01-18 Added clean_dist target
# almamgr  20/06/01  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------


#
# user definable C-compilation flags
USER_CFLAGS = -I../src  

#
# additional include and library search paths
#USER_LIB =

# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = 
EXECUTABLES_L   = acserrTestServer acserrTestClient acserrOldTestServer acserrOldTestClient \
	acserrTestCheckDepth acserrOldTestCheckDepth acserrTestHandlers addDataTest \
	sigSegvTest

#removed: acserrExMgrTest because it depends on baci threads/library
# test should use ACE threads directly or be moved into baci module

#
# <brief description of xxxxx program>
acserrTestHandlers_OBJECTS = acserrTestHandlers
acserrTestHandlers_LIBS = ACSErrTypeTest acserr

acserrExMgrTest_OBJECTS= acserrExMgrTest
acserrExMgrTest_LIBS= acserr ACSErrTypeTest baci

addDataTest_OBJECTS= addDataTest
addDataTest_LIBS= acserr ACSErrTypeTest

memTestRecur_OBJECTS = memTestRecur acserrTestImpl
memTestRecur_LIBS = acserr ACSErrTypeTest acserrTestStubs

memTest_OBJECTS = memTest
memTest_LIBS = acserr ACSErrTypeTest

acserrTestCheckDepth_OBJECTS    = acserrTestCheckDepth
acserrTestCheckDepth_LIBS       = acserr ACSErrTypeTest acserrTestStubs

acserrTestClient_OBJECTS    = acserrTestClient 
acserrTestClient_LIBS       = acserr acserrTestStubs ACSErrTypeTest

acserrTestServer_OBJECTS    = acserrTestServer acserrTestImpl
acserrTestServer_LIBS       = acserr ACSErrTypeTest acserrTestStubs

acserrOldTestCheckDepth_OBJECTS    = acserrOldTestCheckDepth ACSErrOldTypeTestC
acserrOldTestCheckDepth_LIBS       = acserr acserrOldTestStubs

acserrOldTestClient_OBJECTS    = acserrOldTestClient ACSErrOldTypeTestC  ACSErrOldTypeTestS
acserrOldTestClient_LIBS       = acserr acserrOldTestStubs

acserrOldTestServer_OBJECTS    = acserrOldTestServer  ACSErrOldTypeTestC  ACSErrOldTypeTestS acserrOldTestImpl
acserrOldTestServer_LIBS       = acserr acserrOldTestStubs

sigSegvTest_OBJECTS = sigSegvTest 
sigSegvTest_LIBS = acserr

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
SCRIPTS_L	= acserrTestRun.sh startAcserrTest.sh testJUnitACSERRnoRuntime pythonTest
SCRIPTS		= 

PY_MODULES_L = test_AES2Py

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS_L     = acserrTest
TCL_SCRIPTS   =

acserrTest_OBJECTS = acserrTest
acserrTest_TCLSH   = expect

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
# UIF panels (public and local)
# ----------------------------
PANELS   =
PANELS_L = 

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
# INS_ROOT files to be installed
#-------------------------------
INS_ROOT_FILES =
INS_ROOT_DIR   =

#
# other files to be installed
#----------------------------
INSTALL_FILES =

#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

#
# IDL FILES
#

IDL_FILES =  acserrTest ACSErrOldTypeTest acserrOldTest
acserrTestStubs_LIBS = ACSErrTypeTestStubs
ACSErrOldTypeTestStubs_LIBS = acserrStubs
acserrOldTestStubs_LIBS = acserrStubs ACSErrOldTypeTestStubs

#
# Jarfiles and their directories
#

DEBUG=on

JARFILES =  acserrjTest
acserrjTest_DIRS = alma

# ExmplerrType is so far used oly for documentiong purpose
ACSERRDEF = ACSErrTypeTest ExmplErrType

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
all:    do_all
	@if [ $(WIND_BASE) ]; then \
          echo " copy BOTH reference files"; \
	  mkdir -p ref; \
	  cp -f TestList_BOTH.lite TestList.lite; \
	  cp -f ref_BOTH/*.ref ref ;\
	  mkdir -p tmp; \
	else\
          echo " copy WS reference files"; \
	  mkdir -p ref; \
	  cp -f TestList_WS.lite TestList.lite; \
	  cp -f ref_WS/*.ref ref ;\
          mkdir -p tmp; \
        fi
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) *.ior *~ ../include/*~ ../idl/*~ ../*~ core 
	$(RM) TestList.lite ref tmp tatlogs
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#___oOo___
