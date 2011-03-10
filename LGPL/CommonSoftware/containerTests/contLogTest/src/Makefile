#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) European Southern Observatory, 2002
# Copyright by ESO (in the framework of the ALMA collaboration),
# All rights reserved
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
# "@(#) $Id: Makefile,v 1.7 2011/03/10 14:46:54 jagonzal Exp $"
#
# Makefile of ........
#
# who       when        what
# --------  ----------  ----------------------------------------------
# eallaert  2007-11-02  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

USER_CFLAGS =
USER_LIB = 	-lACE \
		-lTAO \
		-lTAO_DsLogAdmin \
		-lTAO_CosNaming \
		-lTAO_IORTable \
		-lTAO_PortableServer \
		-lTAO_Svc_Utils \
		-lTAO_CosTrading \
        -lTAO_CosNotification \
		-lTAO_DynamicAny \
		-lTAO_IFR_Client \
        -lTAO_CosProperty \
		-lacsutil \
		-lcdb \
		-llogging \
	    -lacscomponent \
		-lbaci \
		-lmaci \
		-lmaciClient \
		-lacserr \
		-lm \
        -lloki \
        -lACSErrTypeCommon

#
# Jarfiles: We must distinguish between component implementation classes and other classes.
#
COMPONENTS_JARFILES = contLogTestComp
contLogTestComp_DIRS = \
	alma/contLogTest/TestLogLevelsCompImpl
contLogTestComp_JLIBS = contLogTest_IF

JARFILES = contLogTestClient
contLogTestClient_DIRS = \
	alma/contLogTest/client
contLogTestClient_JLIBS = contLogTest_IF

#
# This will trigger debugging information for Java
# and generation of documentation for generated code.
#
DEBUG = on
COMPONENT_HELPERS=on

#
# Program to build
# Can be overwritten in the call
PROG = 

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# Type-safe log code generation
ACSLOGTSDEF = typeSafeLogs

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = $(PROG) 
EXECUTABLES_L   = 

$(PROG)_OBJECTS            = $(PROG)
$(PROG)_LDFLAGS            =
$(PROG)_LIBS               = C++

#
# Includes (.h) files (public and local)
# ---------------------------------
INCLUDES =	contLogTestImpl.h
INCLUDES_L      = 

#
# Libraries (public and local)
# ----------------------------
LIBRARIES =	contLogTestImpl
LIBRARIES_L     =

contLogTestImpl_OBJECTS = contLogTestImpl
contLogTestImpl_LIBS    = contLogTest_IFStubs ACSErrTypeCommon
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

# Python stuff (public and local)
# ----------------------------
PY_SCRIPTS         = 
PY_SCRIPTS_L       =

PY_MODULES         = 
PY_MODULES_L       =

PY_PACKAGES        = pyContLogTest
PY_PACKAGES_L      =
pppppp_MODULES	   =
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
# IDL Files and flags
# 
IDL_FILES = contLogTest_IF
IDL_TAO_FLAGS =
USER_IDL =

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
	@echo " . . . 'all' done" 

clean : clean_all 
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	cp mainDoxyPage.html ../doc/html/main.html
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#___oOo___


