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
# "@(#) $Id: Makefile,v 1.106 2009/12/17 09:48:16 bjeram Exp $" 
#
# Makefile of baci
#
# who       when      what
# --------  --------  ----------------------------------------------
# bjeram 2003-07-08 removed alarm and monitor stuff
# bjeram 2003-02-25 added baciPpatternImpl
# bjeram 2003-01-23 removed baciROdouble
# almamgr 2002-07-03 Added baciError to baci_COMMON
# almamgr 2002-03-01 Removed S_T generated file.
# bgustafs 2002-02-26 updated for new property types
# bgustafs 2001-07-12 updated for baci 1.11, no debug
# bgustafs 2001-06-26 modified for baci 1.18
# almamgr 2000-07-20 created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------
LINK_FILES= $(addprefix ../idl/, $(addsuffix .xml, $(ACSERRDEF))) \
	$(addsuffix .cpp, $(baci_INCLUDES) $(baci_COMMON)) \
	$(addprefix ../idl/, $(addsuffix .idl, $(IDL_FILES)))

OPTIMIZE=0

# Program to build
# Can be overwritten in the call
USER_CFLAGS =
# -fno-implicit-templates
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
EXECUTABLES     = baci

EXECUTABLES_L   = 

#
# <brief description of xxxxx program>
baci_COMMON =  	baciBACIAction baciBACICallback baciBACIMonitor baciUtil baciBACIProperty baciBACIComponent \
		baciCharacteristicModelImpl baciCharacteristicComponentImpl\
		baciError \
		baci baciTime baciValue baciDB \
                baciRecovery baciCORBA baciDLL \
                baciEvent baciCDBPropertySet \
                baciROdouble baciRWdouble \
                baciROfloat baciRWfloat \
		baciROlongLong baciRWlongLong \
		baciROuLongLong baciRWuLongLong \
                baciROlong baciRWlong \
		baciAlarmSystemMonitorBase \
                baciPpatternImpl baciROpattern baciRWpattern baciAlarmPattern baciAlarmSystemMonitorPattern\
                baciROstring baciRWstring \
		baciROdoubleSeq baciROfloatSeq baciROlongSeq \
                baciRWdoubleSeq baciRWfloatSeq baciRWlongSeq 


baci_GENERATED = 

baci_INCLUDES  = baciCallbackDispatcher

baci_OBJECTS   = $(baci_COMMON) $(baci_GENERATED)

#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

#
# Includes (.h) files (public and local)
# ---------------------------------
INCLUDES        = 
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
# IDL FILES
#
IDL_FILES = 

#
# ERROR FILES
#
ACSERRDEF = 

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

#___oOo___
