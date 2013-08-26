#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.2 2004/09/16 13:35:41 mpasquat Exp $" 
#
# Makefile for testing vltMake (VxWorks environment)
#
# who          when      what
# --------     --------  ----------------------------------------------
# G.Filippi    23/10/94  created
#

#*******************************************************************************
#
# vltMake modular test: UNIX application test scenario
#
#*******************************************************************************

#
# MODULE CODE DESCRIPTION:
# ------------------------

#
# to test additional include and library search paths
USER_INC = -I/user/include 
USER_LIB = -L/user/lib

#
# to test auxiliary areas
INTLIST = /1:/2:/3:/4:/5:/6:/7:/8:/9

#
# C programs (public and local)
# -----------------------------
EXECUTABLES   =  vltMakeTestClibrary
EXECUTABLES_L =  vltMakeTestClibraryLocal

#
# to test link with the C library
vltMakeTestClibrary_OBJECTS = vltMakeTestClibrary
vltMakeTestClibrary_LDFLAGS =
vltMakeTestClibrary_LIBS    = vltMakeTest

#
# to test link with the local C library and to use the same src file twice
vltMakeTestClibraryLocal_OBJECTS = vltMakeTestClibrary
vltMakeTestClibraryLocal_LDFLAGS =
vltMakeTestClibraryLocal_LIBS    = vltMakeTestLocal

#
# Includes (.h) files (public only)
# ---------------------------------
INCLUDES      = 

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       = vltMakeTest
LIBRARIES_L     = vltMakeTestLocal

#
# public library
vltMakeTest_OBJECTS   = vltMakeTestProcedure1 vltMakeTestProcedure2

#
# private library
vltMakeTestLocal_OBJECTS   = vltMakeTestProcedure1 vltMakeTestProcedure2

#
# Scripts (public and local)
# ----------------------------
SCRIPTS   = vltMakeTestScript
SCRIPTS_L = vltMakeTestScriptLocal

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
all:	do_all
	@echo " . . . 'all' done" 

clean : clean_all 
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

execute:
	@$(INTROOT)/vw/bin/vltMakeTestScript
	@        ../bin/vltMakeTestScriptLocal
	@echo " . . . execute done"

#___oOo___
