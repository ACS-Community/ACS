#*******************************************************************************
# ACS project Makefile
#
# "@(#) $Id: Makefile,v 1.6 2012/01/21 22:48:11 tstaig Exp $"
#
# $Log: Makefile,v $
# Revision 1.6  2012/01/21 22:48:11  tstaig
# Backport from branches ACS-9_0_0-windows-B and ACS-9_1_0-windows-B to support
# ACS on Windows under Cygwin. This commit corresponds to several CommonSoftware
# modules.
#
# Revision 1.5  2007/07/16 15:51:25  hsommer
# DEBUG=on to include .java in IDL-generated jar
#
# Revision 1.4  2006/10/18 15:52:30  gchiozzi
# Added schema file for ErrorExplorer.
#
# Revision 1.3  2006/10/09 11:00:40  bjeram
# repalce maciErrTypeComponent with maciErrType
#
# Revision 1.2  2006/09/28 16:01:45  gchiozzi
# Added new components and cleaned up.
#
# Revision 1.1  2006/09/14 14:54:34  gchiozzi
# First checkin of new module with CPP test components.
#
#
# Makefile of acstestcomp
#
# who       when      what
# --------  --------  ----------------------------------------------
# root      25/09/02  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# user definable C-compilation flags
USER_CFLAGS =

#
# additional include and library search paths
USER_INC =
USER_LIB =

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES =  
EXECUTABLES_L   =
#

#
# special compilation flags for single c sources
#TimingExplorer_CFLAGS   = 


$(PROG)_OBJECTS = $(PROG)
$(PROG)_LDFLAGS =
$(PROG)_LIBS = C++

#
# Includes (.h) files (public only)
# ---------------------------------
INCLUDES = acstestcompTimingExplorerImpl.h acstestcompErrorExplorerImpl.h

#
# Libraries (public and local)
# ----------------------------
LIBRARIES = acstestcompTimingExplorer acstestcompErrorExplorer

#
# <brief description of acstestcomp library>
acstestcompTimingExplorer_OBJECTS =  acstestcompTimingExplorerImpl
acstestcompTimingExplorer_LDFLAGS =
acstestcompTimingExplorer_LIBS    = acstestcompStubs logging acsThread acscomponent archiveevents

acstestcompErrorExplorer_OBJECTS =  acstestcompErrorExplorerImpl
acstestcompErrorExplorer_LDFLAGS =
acstestcompErrorExplorer_LIBS    = acstestcompStubs baci

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
CDB_SCHEMAS = ErrorExplorer 

# 
# IDL Files and flags
# 
IDL_FILES = acstestcomp
IDL_TAO_FLAGS =
USER_IDL =
acstestcompStubs_LIBS = acscomponentStubs baciStubs acserrStubs

#
#
#
ACSERRDEF = 

#
# Jarfiles and their directories
#
JARFILES= 
jjj_DIRS=
jjj_EXTRAS= 
#
# java sources in Jarfile on/off
DEBUG=on
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
all:	do_all doxygen
	@echo " . . . 'all' done"

clean:	clean_all
	@echo " . . . clean done"

clean_dist:	clean_all clean_dist_all
	@echo " . . . clean_dist done"

man:	do_man
	@echo " . . . man page(s) done"

install:	install_all
	@echo " . . . installation done"


#___oOo___
