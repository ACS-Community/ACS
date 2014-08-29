#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id$"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# almadev  04/11/11  created
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
EXECUTABLES     = pDataConsumer pDataSupplier
EXECUTABLES_L   = 

#
# <brief description of xxxxx program>
pDataConsumer_OBJECTS   =	pDataConsumer
xxxxx_LDFLAGS   = 
pDataConsumer_LIBS      = corbaNotifyTest_ifStubs TAO_CosNotification_Serv

pDataSupplier_OBJECTS   = 	pDataSupplier
xxxxx_LDFLAGS   =
pDataSupplier_LIBS      = corbaNotifyTest_ifStubs TAO_CosNotification_Serv

corbaNotifyTest_ifStubs_LIBS = baciStubs maciStubs

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
IDL_FILES = corbaNotifyTest_if
TAO_IDLFLAGS =
USER_IDL =


#
# Component jarfiles
#
COMPONENTS_JARFILES = corbaNotifyTestComp
corbaNotifyTestComp_DIRS = acs/benchmark/nc/comp

#
# Jarfiles and their directories
#
JARFILES = corbaNotify
corbaNotify_DIRS = acs/benchmark/nc/client
corbaNotify_EXTRAS=
corbaNotify_JLIBS = pubSubConfigBind


# For expressing dependencies between jarfiles (parallel builds)
corbaNotifyTestComp_JLIBS = corbaNotifyTest_if

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
# Java Entity Classes generation config file
#
XSDBIND = pubSubConfigBind

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


#___oOo___
