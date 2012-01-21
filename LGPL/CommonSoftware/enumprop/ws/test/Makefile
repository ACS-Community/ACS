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
# "@(#) $Id: Makefile,v 1.44 2012/01/21 22:48:11 tstaig Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi 2003-07-14 Added enumpropALMASW2003048.idl test idl
# gchiozzi 2003-05-26 Added creation of ref directory
# bjeram 2002-11-14 added onchangeMonitorTest
# bjeram  07/12/01  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# user definable C-compilation flags
#USER_CFLAGS = 

#
# additional include and library search paths
#USER_INC = 
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
EXECUTABLES_L   = enumpropTestServer testClient testRWClient monitorTest alarmTest onchangeMonitorTest alarmTestSetFFFM

#
# <brief description of xxxxx program>
common_GENERATED = enumpropTestDeviceS enumpropTestDeviceC


enumpropTestServer_OBJECTS =  enumpropTestServer
enumpropTestServer_LIBS = acsContainerServices maci enumpropTestDevice  

testClient_COMMON = testClient
testClient_OBJECTS   = $(testClient_COMMON) $(common_GENERATED)
testClient_LIBS = enumpropTestDevice

testRWClient_COMMON = testRWClient
testRWClient_OBJECTS = $(testRWClient_COMMON) $(common_GENERATED)
testRWClient_LIBS = enumpropTestDevice

monitorTest_COMMON = monitorTest
monitorTest_OBJECTS =  $(monitorTest_COMMON) $(common_GENERATED)
monitorTest_LIBS = enumpropTestDevice

alarmTest_COMMON = alarmTest
alarmTest_OBJECTS=  $(alarmTest_COMMON) $(common_GENERATED)
alarmTest_LIBS = enumpropTestDevice

onchangeMonitorTest_COMMON = onchangeMonitorTest
onchangeMonitorTest_OBJECTS=  $(onchangeMonitorTest_COMMON) $(common_GENERATED)
onchangeMonitorTest_LIBS = enumpropTestDevice

alarmTestSetFFFM_COMMON = alarmTestSetFFFM
alarmTestSetFFFM_OBJECTS = $(alarmTestSetFFFM_COMMON) $(common_GENERATED)
alarmTestSetFFFM_LIBS = enumpropTestDevice
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
LIBRARIES_L     = enumpropTestDevice
enumpropTestDevice_OBJECTS = enumpropTestDeviceImpl 
enumpropTestDevice_LIBS = enumpropTestDeviceStubs baci

#
# <brief description of lllll library>
lllll_OBJECTS   =

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = 
SCRIPTS_L       = startEnumpropTestServer

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     =
TCL_SCRIPTS_L   =

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
IDL_FILES = enumpropTestDevice enumpropALMASW2003048
IDL_TAO_FLAGS =
USER_IDL =
enumpropTestDeviceStubs_LIBS = baciStubs enumpropStdStubs
enumpropALMASW2003048Stubs_LIBS = baciStubs


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
all:	do_all
	@mkdir -p ref
	@mkdir -p tmp
	@if [ $(WIND_BASE) ]; then \
	  cp -f TestList_BOTH.lite TestList.lite; \
          cp -f ref_BOTH/*.ref ref ;\
	else\
          cp -f TestList_WS.lite TestList.lite; \
	  cp -f ref_WS/*.ref  ref ;\
	fi;
	@echo " . . . 'all' done" 

clean : clean_all
	$(RM) ref tatlogs tmp
	$(RM) TestList.lite
	$(RM) EPTESTDEVICE1.ior
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#___oOo___
