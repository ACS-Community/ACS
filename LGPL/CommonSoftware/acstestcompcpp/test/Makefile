#*******************************************************************************
# ACS project Makefile
#
# "@(#) $Id: Makefile,v 1.4 2007/01/09 08:38:37 cparedes Exp $"
#
# Makefile of acstestcomp
#
# $Log: Makefile,v $
# Revision 1.4  2007/01/09 08:38:37  cparedes
# Moving that use a xml with a special character from cdb to here.
#
# Revision 1.3  2006/10/11 15:03:22  gchiozzi
# Added new test program.
#
# Revision 1.2  2006/10/09 14:36:59  bjeram
# repalce maciErrTypeComponent with maciErrType
#
# Revision 1.1  2006/09/14 14:54:34  gchiozzi
# First checkin of new module with CPP test components.
#
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
USER_LIB = -lACE \
        -lTAO \
        -lTAO_DsLogAdmin \
        -lTAO_CosNaming \
        -lTAO_IORTable \
        -lTAO_PortableServer \
        -lTAO_Svc_Utils \
        -lTAO_CosTrading \
        -lTAO_CosNotification \
        -lTAO_DynamicAny \
        -lTAO_IFR_Client \
        -lTAO_CosProperty \
        -lacsutil \
        -lcdb \
        -llogging \
        -lacscomponent \
        -lbaci \
        -lmaci \
        -lmaciErrType \
        -lmaciClient \
        -lacserr \
        -lm \
        -lloki \
	-lacstime

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     =
EXECUTABLES_L   = acstestcompTestClient acstestcompTestComponentActivationErrors

#
# <brief description of xxxxx program>
acstestcompTestClient_OBJECTS   = acstestcompTestClient
acstestcompTestClient_LDFLAGS =
acstestcompTestClient_LIBS = acstestcompStubs  maciClient maciErrType acstime

acstestcompTestComponentActivationErrors_OBJECTS   = acstestcompTestComponentActivationErrors
acstestcompTestComponentActivationErrors_LDFLAGS =
acstestcompTestComponentActivationErrors_LIBS = acstestcompStubs  maciClient maciErrType acstime

acstestcompcppTestCDBCompImpl_OBJECTS       = acstestcompcppTestCDBCompImpl
acstestcompcppTestCDBCompImpl_LIBS          = acstestcompcppTestCDBCompStubs
#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

#
# Includes (.h) files (public only)
# ---------------------------------
INCLUDES        = acstestcompcppTestCDBCompImpl.h

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       = 
LIBRARIES_L     = acstestcompcppTestCDBCompImpl 

#
# <brief description of lllll library>
lllll_OBJECTS   =

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         =
SCRIPTS_L       = TATPrologue TATEpilogue cdbXmlSpecialCharTest cdbInitMount

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
# UIF panels (public and local)
# ----------------------------
PANELS   =
PANELS_L = 

#
# On-Line Database Files
# ----------------------
DBL_CLASSES = 
DBL_BCF =
DBL_FLAGS =
xxxx_DBL_FLAGS =

# 
# IDL Files and flags
# 
IDL_FILES = acstestcompcppTestCDBComp
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

	@mkdir -p tmp
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) *~ CDB/*~ CDB/alma/*~ 
	$(RM) CDB/MACI/*~ CDB/MACI/Components/*~
	$(RM) tatlogs
	$(RM) tmp
	$(RM) sed.scan
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"


#___oOo___
