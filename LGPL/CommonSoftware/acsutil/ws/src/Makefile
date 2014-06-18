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
# who       when      what
# --------  --------  ----------------------------------------------
# almamgr 2003-08-29 Removed acsutil.ild
# gchiozzi 2003-06-02 Added acsIrfeed. Moved here from acsBUILD module
# gchiozzi 2003-05-15 Added acsutilC and acsutilS into library for OffShoot interface
# gchiozzi 2003-05-15 Added new acsutil.idl
# gchiozzi 2002-11-25 Added clean_dist target
# gchiozzi 2002-11-25 created
#

USER_CFLAGS =
USER_LIB =
USER_INC = 

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
EXECUTABLES     = $(PROG)
EXECUTABLES_L   = 

#
# <brief description of xxxxx program>

$(PROG)_OBJECTS            = $(PROG)
$(PROG)_LDFLAGS            =
$(PROG)_LIBS               = C++ CCS


#
# Includes (.h) files (public and local)
# ---------------------------------
INCLUDES        = acsutil.h acsutilWildcard.h acsutilExport.h acsutilThreadInit.h acsutilFindFile.h acsutilLL.h acsutilTempFile.h acsutilPorts.h \
		  acsutilAnyAide.h acsutilTimeStamp.h acsutilORBHelper.h
INCLUDES_L      = 

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       = acsutil
LIBRARIES_L     =

acsutil_OBJECTS	= acsutilWildcard acsutilFindFile acsutilLL acsutilTempFile acsutilPortsImpl acsutilAnyAide acsutilTimeStamp acsutilORBHelper
acsutil_LIBS    = acscommonStubs

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = acsutilTATPrologue acsutilTATEpilogue acsutilTATTestRunner acsutilProfiler acsutilDiffTrap acsutilRedo \
		  acsutilBlock acsutilAwaitContainerStart ipython
SCRIPTS_L       = 

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     =
TCL_SCRIPTS_L   =

#
# Python scripts (public and local)
# ------------------------------
PY_SCRIPTS     = 
PY_SCRIPTS_L   =

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
IDL_FILES =

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
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) *~ ../include/*~ ../dbl/*~ ../idl/*~ ../*~ ../../*~ core
	$(RM) ../doc/*
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#___oOo___
