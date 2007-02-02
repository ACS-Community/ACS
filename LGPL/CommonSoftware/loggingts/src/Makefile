#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id: Makefile,v 1.3 2007/02/02 08:52:46 nbarriga Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# nbarriga  29/01/07  created
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

# log types defined
XML_LOG_DEFS = ACSLogTypeExample

LOG_GEN_XML = $(foreach xml, $(XML_LOG_DEFS), ../idl/$(xml).xml)
LOG_GEN_PY = $(foreach xml, $(XML_LOG_DEFS), ../src/loggingts/$(xml).py)
LOG_GEN_H = $(foreach xml, $(XML_LOG_DEFS), ../include/$(xml).h)
LOG_GEN_CPP = $(foreach xml, $(XML_LOG_DEFS), ../src/$(xml).cpp)


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
INCLUDES        = $(LOG_GEN_H)

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       = $(XML_LOG_DEFS)
LIBRARIES_L     =

#
# <brief description of ACSLogTypeExample library> THIS NEEDS a way to do it automatically
ACSLogTypeExample_OBJECTS   = ACSLogTypeExample

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = loggingtsGenPython loggingtsGenH loggingtsGenCpp
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

PY_PACKAGES        = loggingts
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
IDL_FILES =
IDL_TAO_FLAGS =
USER_IDL =
#
# Jarfiles and their directories
#
JARFILES=simpleLog 
simpleLog_DIRS=alma
jjj_EXTRAS= 
#
# java sources in Jarfile on/off
DEBUG= on
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
INSTALL_FILES = ../config/LTS2Py.xslt ../config/LTS2Cpp.xslt ../config/LTS2H.xslt ../idl/ACSLogTS.xsd

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
all:	do_logts_gen do_all
	@echo " . . . 'all' done" 

clean : clean_all clean_logts_gen
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"



$(LOG_GEN_PY) : $(LOG_GEN_XML)
	- $(AT) for member in $(XML_LOG_DEFS); do \
        echo "== Generating:" $${member}".py out of:" $${member}".xml"; \
        sh ./loggingtsGenPython ../idl/$${member}.xml ../src/loggingts/$${member}.py ; \
        done

$(LOG_GEN_H) : $(LOG_GEN_XML)
	- $(AT) for member in $(XML_LOG_DEFS); do \
        echo "== Generating:" $${member}".h out of:" $${member}".xml"; \
        sh ./loggingtsGenH ../idl/$${member}.xml ../include/$${member}.h ; \
        done

$(LOG_GEN_CPP) : $(LOG_GEN_XML)
	- $(AT) for member in $(XML_LOG_DEFS); do \
        echo "== Generating:" $${member}".cpp out of:" $${member}".xml"; \
        sh ./loggingtsGenCpp ../idl/$${member}.xml ../src/$${member}.cpp ; \
        done

do_logts_gen: $(LOG_GEN_PY) $(LOG_GEN_CPP) $(LOG_GEN_H)
	
clean_logts_gen:
	$(AT) $(RM) $(LOG_GEN_PY) $(LOG_GEN_CPP) $(LOG_GEN_H)

#___oOo___
