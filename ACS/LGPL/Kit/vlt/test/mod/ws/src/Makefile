#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.2 2004/09/16 13:38:04 mpasquat Exp $" 
#
# Makefile for testing vltMake (VxWorks environment)
#
# who          when      what
# --------     --------  ----------------------------------------------
# G.Filippi    27/10/94  created
#

#*******************************************************************************
#
#  vltMake modular test: UNIX application development scenario
#
#*******************************************************************************

#
# MODULE CODE DESCRIPTION:
# ------------------------

# to test used definable C-compilation flags
USER_CFLAGS = -DMYFLAG

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
EXECUTABLES   =  vltMakeTestUSER_CFLAG vltMakeTestCCSusage vltMakeTestClibrary
EXECUTABLES_L =  vltMakeTestClibraryLocal

#
# to test USER_CFLAG
vltMakeTestUSER_CFLAG_OBJECTS = vltMakeTestUSER_CFLAG
vltMakeTestUSER_CFLAG_LDFLAGS =
vltMakeTestUSER_CFLAG_LIBS    = 

#
# to test a CCS application and RTAP not required message
vltMakeTestCCSusage_OBJECTS = vltMakeTestCCSusage
vltMakeTestCCSusage_LDFLAGS =
vltMakeTestCCSusage_LIBS    = CCS RTAP

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
INCLUDES      = vltMakeTest.h

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
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     = vltMakeTestTCLScript
TCL_SCRIPTS_L   = vltMakeTestTCLScriptLocal

#
# <brief description of tttttt tcl-script>
vltMakeTestTCLScript_OBJECTS  = vltMakeTestTCLScript
vltMakeTestTCLScript_TCLSH    =
vltMakeTestTCLScript_LIBS     = vltMakeTestTCLlib

#
# TCL libraries (public and local)
# ------------------------------
TCL_LIBRARIES   = vltMakeTestTCLlib
TCL_LIBRARIES_L = vltMakeTestTCLlibLocal

#
# <brief description of tttlll library>
vltMakeTestTCLlib_OBJECTS = vltMakeTestTCLProcedure1 vltMakeTestTCLProcedure2

#
# <brief description of tttlll library>
vltMakeTestTCLlibLocal_OBJECTS = vltMakeTestTCLProcedure1 vltMakeTestTCLProcedure2

#
# UIF panels (public and local)
# ----------------------------
PANELS   = vltMakeTestPanel1 vltMakeTestPanel2 vltMakeTestPanel3
PANELS_L = 

#
# On-Line Database Files
# ----------------------
DBL_CLASSES = vltMakeTestAUTOLOAD  vltMakeTestCPU_CLASS  vltMakeTestCCD

DBL_BCF = vltMakeTestBranch1 vltMakeTestBranch2
DBL_LOAD = vltMakeTesttest32 vltMakeTesttest27
DBL_FLAGS = -DMY_FLAG=on -DXYZ
test32_DBL_FLAGS = -DLCU=no
test27_DBL_FLAGS = -DWS=yes

#
# man pages to be done
# --------------------
MANSECTIONS = 1 2 3 4 5 6 7 8 n
MAN1 =  $(addsuffix .c, $(EXECUTABLES))
MAN2 =  $(addsuffix .c, $(EXECUTABLES))
MAN3 =  $(addsuffix .c, $(EXECUTABLES))
MAN4 =  $(addsuffix .c, $(EXECUTABLES))
MAN5 =  $(addsuffix .c, $(EXECUTABLES))
MAN6 =  $(addsuffix .c, $(EXECUTABLES))
MAN7 =  $(addsuffix .c, $(EXECUTABLES))
MAN8 =  $(addsuffix .c, $(EXECUTABLES)) 
MANn =  $(addsuffix .c, $(EXECUTABLES))

#
# local man pages
# ---------------
MANl =  $(addsuffix .c, $(EXECUTABLES))

#
# Convert ASCII document into Framemaker MIF format
# -------------------------------------------------
ASCII_TO_MIF = $(addsuffix .c, $(EXECUTABLES))

#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
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
all:	do_all
	@echo " . . . 'all' done" 

clean : clean_all 
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

db: db_all
	@echo " . . . ../DB done"

execute:
	@$(INTROOT)/bin/vltMakeTestClibrary
	@        ../bin/vltMakeTestClibraryLocal
	@$(INTROOT)/bin/vltMakeTestTCLScript
	@$(INTROOT)/bin/vltMakeTestCCSusage
	@$(INTROOT)/bin/vltMakeTestScript
	@        ../bin/vltMakeTestScriptLocal
	@$(INTROOT)/bin/vltMakeTestUSER_CFLAG
	@echo " . . . execute done"

#___oOo___
