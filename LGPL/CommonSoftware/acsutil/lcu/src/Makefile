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
# "@(#) $Id: Makefile,v 1.29 2006/01/13 09:21:15 bjeram Exp $" 
#
# Makefile of acsutil
#
# who       when      what
# --------  --------  ----------------------------------------------
# david 2003-06-04 fixed linking of generated files.
# bjeram 2003-05-22 added IDL file acsuril + procedure to links idl too
# gchiozzi 2002-11-25 Added check on WIND_BASE
# bgustafs 2002-04-05  acsutilTempFile added
# jibsen 2002-04-04   acsutilArgUnpack.h file added
# bjeram 2001-12-06 added acsFindFile
# bgustafs 2001-07-11 created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

OPTIMIZE=0

# Program to build
# Can be overwritten in the call

PROG = 



#
# additional include and library search paths
#USER_LIB = 

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = acsutil $(PROG)

EXECUTABLES_L   = 

#
# <brief description of xxxxx program>
acsutil_COMMON    = acsutilWildcard acsutilFindFile acsutilLL acsutilTempFile acsutilPortsImpl acsutilAnyAide acsutilTimeStamp acsutilORBHelper
acsutil_GENERATED = 
acsutil_OBJECTS   = $(acsutil_COMMON) $(acsutil_GENERATED) vsprintf

$(PROG)_OBJECTS            = $(PROG)
$(PROG)_LDFLAGS            =
$(PROG)_LIBS               = $(TESTLIBS) C++ CCS

#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

#
# Includes (.h) files (public and local)
# ---------------------------------
INCLUDES        = acsutilArgUnpack.h vsprintf.h
INCLUDES_L      = 
#
# Libraries (public and local)
# ----------------------------
LIBRARIES       =
LIBRARIES_L     =

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

#
# IDL Files and flags
#
IDL_FILES =

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
#>>>>> END OF standard rules

#
# CHOSE PLATFORM
# --------------
# Default is UNIX, for VxVorks application next line MUST BE UNCOMMENTED
MAKE_VXWORKS = on   

ifdef WIND_BASE
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
all:	do_links do_all
	@echo " . . . 'all' done" 

clean : clean_links clean_all 
	$(RM) *~ ../include/*~ ../dbl/*~ ../idl/*~ ../*~ ../../*~ core
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

else
all:    
	@echo " WIND_BASE not defined. Nothing to do 'all'!"
clean : 
	@echo " WIND_BASE not defined. Nothing to 'clean' !"

clean_dist : 
	@echo " WIND_BASE not defined. Nothing to 'clean_dist' !"

man   : 
	@echo " WIND_BASE not defined. Nothing to 'man' !"

install : 
	@echo " WIND_BASE not defined. Nothing to 'install' !"

endif

#Common stuff
COMMON_SOURCES = \
	$(foreach exe, $(acsutil_COMMON), $(exe).cpp)
COMMON_SOURCES_ORIG = \
	$(foreach exe, $(acsutil_COMMON), ../../ws/src/$(exe).cpp)

$(COMMON_SOURCES) : $(COMMON_SOURCES_ORIG)
	- $(AT) if [ ! -h $@ ]; then echo "== Linking source: $@"; ln -s ../../ws/src/$@ $@; fi

COMMON_IDL = \
	$(foreach exe, $(IDL_FILES), ../idl/$(exe).idl)
COMMON_IDL_ORIG = \
	$(foreach exe, $(IDL_FILES), ../../ws/idl/$(exe).idl)

$(COMMON_IDL) : $(COMMON_IDL_ORIG)
	- $(AT) if [ ! -h $@ ]; then echo "== Linking source: $@"; ln -s ../../ws/idl/$@ $@; fi

do_links: $(COMMON_SOURCES) $(COMMON_IDL)

clean_links:
	$(AT) $(RM) $(COMMON_SOURCES) $(COMMON_IDL)
#___oOo___
