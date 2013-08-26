#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: Makefile,v 1.104 2012/01/21 22:48:11 tstaig Exp $" 
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi 2003-02-06 Creating ref dir because it is not extracted by CVS.
# rgeorgie 2002-12-05 Commented out CCS libraries
# gchiozzi 2002-04-05 Added Fridge scripts
# gchiozzi 2002-01-21 Added clean_dist target.
# almamgr 2000-07-20 created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

USER_CFLAGS =
USER_LIB =

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
EXECUTABLES_L   = acsexmplTestMemory \
		  acsexmplClientTower

# ---> CDB_Test in test directory


#
# <brief description of xxxxx program>

$(PROG)_OBJECTS            = $(PROG)
$(PROG)_LDFLAGS            =
$(PROG)_LIBS               = C++

acsexmplTestMemory_OBJECTS = acsexmplTestMemory
acsexmplTestMemory_LIBS    = acsexmplMountStubs maciClient

acsexmplClientTower_OBJECTS = acsexmplClientTower
acsexmplClientTower_LIBS    = acsexmplBuildingStubs maciClient

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
SCRIPTS_L       = acsexmplFridgeOn.sh \
                  acsexmplFridgeOpenClose.sh \
                  acsexmplFridgeOpenOnOffClose.sh \
                  acsexmplTestRun.sh \
		  acsexmplTATPrologue


#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     = acsexmplTest
TCL_SCRIPTS_L   =

acsexmplTest_OBJECTS = acsexmplTest
acsexmplTest_TCLSH   = expect


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
all:	checkCompleteCdb do_all 
	@mkdir -p tmp
	@mkdir -p ref
	@if [ $(WIND_BASE) ]; then \
	  cp TestList_BOTH.lite TestList.lite ; \
	  cp ref_BOTH/*.ref ref/ ;\
	else\
	  cp TestList_WS.lite TestList.lite ; \
	  cp ref_WS/*.ref ref/ ;\
	fi;
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) *~
	$(RM) ref tatlogs
	$(RM) tmp tatlogs
	$(RM) TestList.lite
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	$(RM)  ../../lcu/object/*
	$(RM) -rf ref
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

# This points to the CDB base directory, i.e. that one that has
# the CDB subdirectory.
CDB_BASE=$(PWD)
checkCompleteCdb:
	cdbChecker -v $(CDB_BASE)

#___oOo___

