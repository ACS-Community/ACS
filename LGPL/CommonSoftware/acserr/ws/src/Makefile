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
# "@(#) $Id: Makefile,v 1.86 2010/10/26 18:02:07 javarias Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi 2002-12-12 Removed reference to fnd and CCD libraries
# msekoran 2002-02-26 Added ACSErrTypeCORBA IDLs.
# gchiozzi 2002-02-26 Changed order in IDL_FILES
# msekoran 2002-02-12 Added ACSErrTypeMonitor, ACSErrTypeAlarm IDLs.
# gchiozzi 2002-01-18 Added clean_dist target
# almamgr  20/06/01  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------
PY_PACKAGES = ACSErrorChecker

DEBUG=on

#
# user definable C-compilation flags

#
# additional include and library search paths
USER_LIB = -lTAO -lACE 

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
INCLUDES        = acserr.h acserrExceptionManager.h acserrLegacy.h acserrACSbaseExImpl.h \
		 acserrHandlers.h acserrHandlersErr.h ErrorSystemErrType.h

 
#
# Libraries (public and local)
# ----------------------------
LIBRARIES       = acserr
LIBRARIES_L     =

#
# <brief description of lllll library>
acserr_OBJECTS = acserr acserrC acserrS acserrExceptionManager \
		 acserrLegacy acserrACSbaseExImpl \
		 acserrHandlersErrC acserrHandlersErrS  acserrHandlersErr acserrHandlers ErrorSystemErrType
acserr_LIBS = logging loki acsutil

ErrorSystemErrTypeStubs_LIBS := acserrStubs
acserrHandlersErrStubs_LIBS := acserrStubs
#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = acserrGenCpp acserrGenJava acserrGenIDL acserrGenPython updateErrDefs.sh
SCRIPTS_L       =

#
# other files to be installed
#----------------------------
INSTALL_FILES = ../config/AES2H.xslt ../config/AES2CPP.xslt ../config/AES2IDL.xslt ../config/AES2Java.xslt ../config/AES2Py.xslt ../idl/ACSError.xsd

#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

#
# IDL FILES
#
# WARNING: Here the order is important, since it is the IDL stub generation
#          and is therefore essential when building stubs that 
#          depend on other stubs.
#          acserr.idl depends on the error types.

# we create acserr and acserrHandlersErr because we want that the makfile takes care for install
IDL_FILES =  acserr acserrHandlersErr ErrorSystemErrType


XSDBIND = ACSError
XSDBIND_INCLUDE = commontypes

#
# Jarfiles and their directories
#
JARFILES = acserrj
acserrj_JLIBS=acserr
acserrj_DIRS = alma/acs

# Here we have to generate the code for errors by hand because the acserr library is not available yet
# and because we want to make err system mor compact (w/o too much err type libraries)
ERRCODE_GEN = acserrHandlersErr ErrorSystemErrType

#ACSERRDEF = ErrorSystemErrType
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

ERRCODE_GEN_XML = $(foreach xml, $(ERRCODE_GEN), ../idl/$(xml).xml)
ERRCODE_GEN_IDL = $(foreach xml, $(ERRCODE_GEN), ../idl/$(xml).idl)
ERRCODE_GEN_H = $(foreach xml, $(ERRCODE_GEN), ../include/$(xml).h)
ERRCODE_GEN_CPP = $(foreach xml, $(ERRCODE_GEN), ../src/$(xml).cpp) $(ERRCODE_GEN_H)


MAKEDIRTMP := $(shell searchFile include/acsMakefile)
ifneq ($(MAKEDIRTMP),\#error\#)
   MAKEDIR := $(MAKEDIRTMP)/include
   include $(MAKEDIR)/acsMakefile
endif

#
# TARGETS
# -------
all:	do_errcode_gen do_all
	@echo " . . . 'all' done" 

clean : clean_all clean_errcode_gen
	$(RM) *~ ../doc/*.pcode ../include/*~ ../dbl/*~ ../*~ ../../*~ core
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"


$(ERRCODE_GEN_IDL) : $(ERRCODE_GEN_XML)
	$(AT) for member in $(ERRCODE_GEN); do \
	echo "== Generating:" $${member}".idl out of:" $${member}".xml"; \
		sh ./acserrGenIDL  ../idl/$${member}.xml ../idl/$${member}.idl ; \
		done

$(ERRCODE_GEN_CPP) : $(ERRCODE_GEN_XML)
	$(AT) for member in $(ERRCODE_GEN); do \
		echo "== Generating:" $${member}".cpp and:" $${member}".h out of:" $${member}".xml"; \
		sh ./acserrGenCpp ../idl/$${member}.xml ../src/$${member}.cpp ../include/$${member}.h ; \
		done

do_errcode_gen :  $(ERRCODE_GEN_IDL) $(ERRCODE_GEN_CPP) 

clean_errcode_gen:
	$(AT) $(RM) $(ERRCODE_GEN_IDL) $(ERRCODE_GEN_CPP)

#___oOo___
