#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.20 2012/01/24 01:00:05 tstaig Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# bjeram  2004-09-17  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# user definable C-compilation flags


#
# additional include and library search paths
USER_INC = -I$(INSTALL_ROOT)/include/xercesc/dom
USER_LIB =

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = standaloneTaskTest paramTaskTest
EXECUTABLES_L   = 

#
# <brief description of xxxxx program>

standaloneTaskTest_LIBS = characteristicTask execComponentTestImpl ACSErrTypeCommon
# could be also:
# standaloneTaskTest_OBJECTS   = execComponentTestImpl
# standaloneTaskTest_LIBS = characteristicTask ACSErrTypeCommon
# but since we already have library jut link them togther


paramTaskTest_LIBS = parameterTask ACSErrTypeCommon paramTaskTestImpl acsXercesUtilities
# could be :
# paramTaskTest_OBJECTS   = paramTaskTestImpl 
# paramTaskTest_LIBS = parameterTask ACSErrTypeCommon
# but we have already the library created so why shouldn't we use it

xxxxx_OBJECTS   =	
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
LIBRARIES       = execComponentTestImpl characteristicTaskTestImpl paramTaskTestImpl
LIBRARIES_L     = 

#
# <brief description of lllll library>
execComponentTestImpl_OBJECTS   = execComponentTestImpl
execComponentTestImpl_LIBS = ACSErrTypeCommon taskComponentStubs acscomponent archiveevents

#
characteristicTaskTestImpl_OBJECTS = characteristicTaskTestImpl
characteristicTaskTestImpl_LIBS = ACSErrTypeCommon baci characteristicTaskStubs

#
paramTaskTestImpl_OBJECTS = paramTaskTestImpl
paramTaskTestImpl_LIBS = ACSErrTypeCommon parameterTask

# Scripts (public and local)
# ----------------------------
SCRIPTS         =
SCRIPTS_L       = taskTATEnv paramTaskTest.xml runParamTaskTest.sh taskRunnerCharacteristicTaskTest.sh

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
CDB_SCHEMAS = 

# 
# IDL Files and flags
# 
IDL_FILES = paramTaskTest
IDL_FILES = 
IDL_TAO_FLAGS =
USER_IDL =
#
# Jarfiles and their directories
#
JARFILES= 
jjj_DIRS=
jjj_EXTRAS= 
#
# java sources in Jarfile on/off
DEBUG= 
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
ifdef ACSROOT
    MAKEDIR  = $(shell if [ -f $(INTROOT)/include/acsMakefile ];  then \
                     echo $(INTROOT)/include; \
	           else \
	             echo $(ACSROOT)/include; \
		   fi;)
    include $(MAKEDIR)/acsMakefile
else
    MAKEDIR  = $(shell if [ -f $(INTROOT)/include/vltMakefile ];  then \
                     echo $(INTROOT)/include; \
	           else \
	             echo $(VLTROOT)/include; \
		   fi;)
    include $(MAKEDIR)/vltMakefile
endif

#
# TARGETS
# -------
all:	do_all
	@mkdir -p tmp
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) *~
	$(RM) tmp tatlogs
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"


#___oOo___
