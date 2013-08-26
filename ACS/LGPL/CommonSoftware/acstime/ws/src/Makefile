#*******************************************************************************
# NRAO - ACS project
#
# "@(#) $Id: Makefile,v 1.30 2012/01/21 22:48:11 tstaig Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi 2003-09-01 Removed test target
# david  2003-07-04  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# user definable C-compilation flags
#USER_CFLAGS = 

DEBUG=on

#
# additional include and library search paths
USER_INC = -I$(PYTHONINC)
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
INCLUDES        = 	acstimeClockImpl.h \
			acstimeTimerImpl.h \
			acstimeDevIOTime.h acstimeTimeUtil.h \
			acstimeDurationHelper.h acstimeEpochHelper.h \
			acstime.h \
			acstimeProfiler.h


#
# Libraries (public and local)
# ----------------------------
LIBRARIES       = acstime acsclock acstimer _acstimeSWIG 
LIBRARIES_L     =

acstime_OBJECTS   =	acstimeTimeUtil \
			acstimeDurationHelper acstimeEpochHelper \
			acstimeProfiler \
			acstimeC acstimeS
acstime_LIBS      =     baci ACSTimeError maciErrType

acsclock_OBJECTS   =	acstimeClockImpl
acsclock_LIBS      =    baci ACSTimeError acstime


acstimer_OBJECTS   =	acstimeTimerImpl
acstimer_LIBS      =    baci ACSTimeError acstime


#Quick fix a Sun-SWIG problem is to NOT compile acstimeSWIG_wrap for Sun
_acstimeSWIG_OBJECTS   = $(shell if [ $(shell uname) = Linux -o $(shell uname) = $(CYGWIN_VER) ]; then echo 'acstimeSWIG_wrap '; fi;) \
			 acstimeTimeUtil \
			 acstimeDurationHelper acstimeEpochHelper \
			 acstimeC acstimeS
_acstimeSWIG_LIBS      = baci ACSTimeError maciErrType python2.6



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
# Python stuff (public and local)
# ----------------------------
PY_SCRIPTS         =
PY_SCRIPTS_L       =

PY_MODULES         = acstimeSWIG
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
# UIF panels (public and local)
# ----------------------------
PANELS   =
PANELS_L = 

# Self-explanatory
CDB_SCHEMAS = CLOCK

# 
# IDL Files and flags
# 
IDL_FILES = acstime
IDL_TAO_FLAGS =
USER_IDL =
acstimeStubs_LIBS = baciStubs ACSTimeErrorStubs

#ACS Error System target
ACSERRDEF = ACSTimeError

#
# Jarfiles and their directories
#
JARFILES = jacstime
jacstime_JLIBS=acstime
jjj_DIRS = 

jacstime_DIRS = alma

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
	$(RM) *~ ../idl/*~ ../include/*~ ../test/*~
	$(RM) ../config/CDB/schemas/*~
	$(RM) .purify
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@$(RM) $(INSTALL_ROOT)/lib/python/site-packages/_acstimeSWIG.$(SHLIB_EXT)
	@cp ../lib/lib_acstimeSWIG.$(SHLIB_EXT) $(INSTALL_ROOT)/lib/python/site-packages/_acstimeSWIG.$(SHLIB_EXT)
	@echo " . . . installation done"

acstime_swig:
	@swig -c++ -python ../config/acstimeSWIG.i
	@mv ../config/acstimeSWIG_wrap.cxx acstimeSWIG_wrap.cpp
	@mv ../config/acstimeSWIG.py .

#___oOo___
