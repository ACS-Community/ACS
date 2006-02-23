#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.2 2004/09/16 13:39:17 mpasquat Exp $" 
#
# Makefile for testing vltMake (ws environment)
#
# who          when      what
# --------     --------  ----------------------------------------------
# G.Filippi    27/10/94  created
#

#*******************************************************************************
#
# vltMake modular test: UNIX application development test
#
#*******************************************************************************

#
# MODULE CODE DESCRIPTION:
# ------------------------


#
# C programs (public and local)
# -----------------------------
EXECUTABLES   =
EXECUTABLES_L =  vltMakeTestClibrary

#
# to test link with the C library
vltMakeTestClibrary_OBJECTS = vltMakeTestClibrary
vltMakeTestClibrary_LDFLAGS =
vltMakeTestClibrary_LIBS    = vltMakeTest


#
# Includes (.h) files (public only)
# ---------------------------------
INCLUDES      = 

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       =
LIBRARIES_L     = vltMakeTest

#
# public library
vltMakeTest_OBJECTS   = vltMakeTestProcedure1 vltMakeTestProcedure2


#
# man pages to be done
# --------------------
MANSECTIONS = 

#
# local man pages
# ---------------
MANl = 

#
# Convert ASCII document into Framemaker MIF format
# -------------------------------------------------
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
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

execute:
	@../bin/vltMakeTestClibrary
	@echo " . . . execute done"

#___oOo___
