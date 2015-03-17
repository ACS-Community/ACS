#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.7 2005/01/31 21:45:24 dfugate Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# oat  2003-09-29  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------
# List of files to be linked 
#!# (ICT-3855: commented out) LINK_FILES = ../test/acssampFullNCTest.cpp
#		../test/acssampOnlyNCServer.cpp \
#		../test/acssampOnlyNCClient.cpp

#

#
# user definable C-compilation flags
#USER_CFLAGS = 

#
# additional include and library search paths
#USER_INC = 
#USER_LIB = 

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     =
EXECUTABLES_L   = acssampFullNCTest
#			acssampOnlyNCServer \
#			acssampOnlyNCClient

#
# <brief description of xxxxx program>
acssampFullNCTest_OBJECTS   = acssampFullNCTest

#acssampOnlyNCServer_OBJECTS   = acssampOnlyNCServer	
#acssampOnlyNCClient_OBJECTS   = acssampOnlyNCClient

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
# IDL Files and flags
#
IDL_FILES =
IDL_TAO_FLAGS =
USER_IDL =

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
#>>>>> END OF standard rules

#
# CHOSE PLATFORM
# --------------
# This is a LCU Makefile, so for VxVorks applications we set:
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
all:	do_all
	@echo " . . . 'all' done" 

clean : clean_all 
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

test:
	tat -nc -v
	@echo " . . . test done"

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


#___oOo___
