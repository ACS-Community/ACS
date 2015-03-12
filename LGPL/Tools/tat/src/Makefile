#*******************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.84 2011/10/11 13:21:16 psivera Exp $"
#
# Makefile of tat: tools for automated testing
#
# who       when      what
# --------  --------  ----------------------------------------------
# eallaert 2014-10-22 Set NOCCS if ACSROOT is defined;
# sfeyrin  2010-09-16 Removed eccsTestDriver, eccsTestSpawner;
#                     Added include acsMakefile when ACS_ROOT define
# sfeyrin  2008-12-19 Removed "-f" from expect in *_TCLSH
# eallaert 2003-06-11 Removed "-f" from *_TCLSH (if seqSh or tcl)
# gchiozzi 2002-12-10 Added eccsTestDriver and eccsTestSpawner tools
# pforstma  07/07/95  created
# gfilippi  07/05/98  do not use vcc for NOCCS (VLTSW980249)
# fcarbogn  23/06/98  Upgrade to New CCS_LITE

#
#
# MODULE CODE DESCRIPTION:
# ------------------------

# Do not use vcc for NOCCS installation
#
# WARNING: On a standard ACS installation without CCS, ALWAYS SET NOCCS=1
# 

# ICT-3707: use of NOCCS has been removed from Kit/acs and build-files.
# If we're running on an ACS-system, set it here locally.
ifdef ACSROOT
    NOCCS=1
endif

ifndef NOCCS
    VCC_LIB = vcc
endif

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     = tat \
		  tatRemExec \
		  tatCleanShm \
		  tatGetClock \
		  tatEnvStatus \
                  tatTestSpawner



tat_OBJECTS  = tat
tat_LIBS = tatlib $(VCC_LIB)
tat_TCLSH = seqSh

tatRemExec_OBJECTS  = tatRemExec
tatRemExec_TCLSH = expect

tatCleanShm_OBJECTS  = tatCleanShm
tatCleanShm_LIBS = tatlib
tatCleanShm_TCLSH = tcl

tatGetClock_OBJECTS  = tatGetClock
tatGetClock_LIBS = tatlib
tatGetClock_TCLSH = tcl

tatEnvStatus_OBJECTS  = tatEnvStatus
tatEnvStatus_LIBS = tatlib $(VCC_LIB)
tatEnvStatus_TCLSH = seqSh

tatTestSpawner_OBJECTS  =    tatTestSpawner
tatTestSpawner_TCLSH    =    seqSh

# TCL libraries (public and local)
# ------------------------------
TCL_LIBRARIES   = tatlib

# tat: public libary of tat
tatlib_OBJECTS =tatLib
ifndef NOCCS
tatlib_OBJECTS := $(tatlib_OBJECTS) \
		  tatMakeLCUEnv \
		  tatMakeRTAPEnv \
		  tatCleanLCUEnv \
	          tatCleanRTAPEnv \
	          tatGetRTAPEnv \
                  tatGetLCUEnv \
                  tatGetQsemuEnv \
		  tatMakeQsemuEnv \
		  tatCleanQsemuEnv
endif


# man pages to be done
# --------------------
MANSECTIONS = 1
MAN1 = tat.tcl tatCleanShm.tcl tatEnvStatus.tcl tatTestSpawner.tcl
ifndef NOCCS
MAN1 := $(MAN1) tatMakeLCUEnv.tcl tatMakeRTAPEnv.tcl tatCleanLCUEnv.tcl                tatCleanRTAPEnv.tcl tatGetRTAPEnv.tcl tatGetQsemuEnv.tcl tatGetLCUEnv.tcl 
endif

#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

#
# INCLUDE STANDARDS
# -----------------

ifdef ACSROOT
MAKEDIRTMP := $(shell searchFile include/acsMakefile)
ifneq ($(MAKEDIRTMP),\#error\#)
   MAKEDIR := $(MAKEDIRTMP)/include
   include $(MAKEDIR)/acsMakefile
endif
else
MAKEDIR = $(VLTROOT)/include
include $(MAKEDIR)/vltMakefile
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

db : db_all
	@echo " . . . ../DB done"


#___oOo___
