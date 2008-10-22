#*******************************************************************************
# PPPPPPPP
#
# "@(#) $Id: Makefile,v 1.4 2008/10/22 05:04:11 javarias Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# javarias  01/09/08  created
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
USER_INC = -I$(DDS_ROOT)
USER_LIB = -L$(DDS_ROOT)/lib 

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
#xxxxx_OBJECTS   = 	
#xxxxx_LDFLAGS   = 
#xxxxx_LIBS      = 

#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

#
# Includes (.h) files (public only)
# ---------------------------------
INCLUDES        =  DDSHelper.h DDSPublisher.h DDSSubscriber.h \
						 DataReaderListener.h acsddsncDataReaderListener.h \
						 NCBenchmarkImpl.h

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       = acsddsnc NCBenchTypeSupport NCBenchmarkImpl
LIBRARIES_L     =

#
# <brief description of acsddsnc library>
acsddsnc_OBJECTS  = DDSHelperImpl DDSSubscriberImpl DataReaderListenerImpl
acsddsnc_LIBS		= OpenDDS_Dcps SimpleTcp
#
# <brief description of acsddsncBench library>
NCBenchmarkImpl_OBJECTS = NCBenchmarkImpl
NCBenchmarkImpl_LIBS    = NCBenchmarkStubs OpenDDS_Dcps SimpleTcp

#
# <brief description of acsddsncBench library>
NCBenchTypeSupport_OBJECTS = $(DDS_TYPE_FILES)
NCBenchTypeSupport_LIBS		= NCBenchmarkStubs OpenDDS_Dcps SimpleTcp

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
IDL_FILES = NCBenchmark
DDS_IDL_FILES = NCBenchmark
DDS_TYPES = Message
TAO_IDLFLAGS = -Gdcps
USER_IDL = -I$(DDS_ROOT)
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

MAKEDIRTMP := $(shell searchFile include/acsMakefile)
ifneq ($(MAKEDIRTMP),\#error\#)
   MAKEDIR := $(MAKEDIRTMP)/include
   include $(MAKEDIR)/acsMakefile
endif

#
# TARGETS
# -------
all: build_dds do_all
	@echo " . . . 'all' done" 

clean : clean_dds clean_all 
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_dds install_all
	@echo " . . . installation done"

DDS_IDLS=$(foreach idl, $(DDS_IDL_FILES), ../idl/$(idl).idl)
DDS_TYPE_IDLS=$(foreach type, $(DDS_TYPES), $(type)TypeSupport.idl)
DDS_TYPE_FILES=$(foreach type, $(DDS_TYPES), $(type)TypeSupportC $(type)TypeSupportS $(type)TypeSupportImpl)
DDS_INCLUDE_FILES=$(foreach type, $(DDS_TYPES), $(type)TypeSupportC.h $(type)TypeSupportC.inl $(type)TypeSupportImpl.h $(type)TypeSupportS.h $(type)TypeSupportS.inl)

build_dds: $(DDS_TYPE_IDLS)

$(DDS_TYPE_IDLS):
	@echo "==== Generating DDS idls"
	cp -r $(DDS_IDLS) .
	$(foreach idl, $(DDS_IDL_FILES), $(DDS_ROOT)/bin/dcps_ts.pl $(idl).idl)
	@echo "==== Processing DDS idls: $(DDS_TYPE_IDLS)"
	$(foreach idl, $(DDS_TYPE_IDLS), $(ACE_ROOT)/TAO/TAO_IDL/tao_idl $(IDL_PATH) $(USER_IDL) $(TAO_IDLFLAGS) $(USER_IDL) $(idl))
#	rm -rf $(foreach idl, $(DDS_IDL_FILES), $(idl).idl)

clean_dds:
	rm -rf *TypeSupport*
	rm -rf $(DDS_IDL_FILES).idl $(DDS_TYPE_IDLS)

install_dds: $(DDS_TYPES_IDLS)
	cp $(DDS_INCLUDE_FILES) $(INTROOT)/include
#	$(foreach file, $(DDS_INCLUDE_FILES), chmod 775 $(file))

#___oOo___
