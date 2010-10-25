#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2010 
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
# "@(#) $Id: Makefile,v 1.7 2010/10/25 20:15:47 javarias Exp $"
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
# USER_CFLAGS = 

#
# additional include and library search paths
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
			  -lmaciClient \
			  -lacserr \
			  -lm \
			  -lloki \
			  -lACSErrTypeCommon \
			  -lacsnc \
			  -lOpenDDS_Dcps \
			  -lSimpleTcp

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES_L     = simpleExampleNCClient simpleExampleDDSClient ncConsumer ddsSubscriber ncSlowConsumer ddsSlowSubscriber

simpleExampleDDSClient_OBJECTS   = simpleExampleDDSClient
simpleExampleDDSClient_LDFLAGS   =  
simpleExampleDDSClient_LIBS      = acsddsnc maciClient \
                                   SimpleExampleStubs simpleMessageTypeSupportStubs \
                                   acsddsncTypeSupport

simpleExampleNCClient_OBJECTS   = 	simpleExampleNCClient
simpleExampleNCClient_LDFLAGS   =  
simpleExampleNCClient_LIBS      =  acsnc maciClient SimpleExampleStubs 

ncConsumer_OBJECTS   = 	ncConsumer
ncConsumer_LDFLAGS   =  
ncConsumer_LIBS      =  acsnc maciClient NCBenchmarkStubs

ddsSubscriber_OBJECTS   = ddsSubscriber
ddsSubscriber_LDFLAGS   = 
ddsSubscriber_LIBS      = acsddsnc NCBenchmarkStubs MessageTypeSupportStubs acsddsncTypeSupport

ncSlowConsumer_OBJECTS   = 	ncSlowConsumer
ncSlowConsumer_LDFLAGS   =  
ncSlowConsumer_LIBS      =  acsnc maciClient NCBenchmarkStubs

ddsSlowSubscriber_OBJECTS   = ddsSlowSubscriber
ddsSlowSubscriber_LDFLAGS   = 
ddsSlowSubscriber_LIBS      = acsddsnc NCBenchmarkStubs MessageTypeSupportStubs acsddsncTypeSupport

#
# Includes (.h) files (public only)
# ---------------------------------
INCLUDES      =  

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       = acsddsncTypeSupport simpleExampleNCImpl simpleExampleDDSImpl \
		  NCBenchmarkImpl DDSNCBenchmarkImpl testCDBProps
LIBRARIES_L     =

#
# <brief description of simpleExample library>
simpleExampleDDSImpl_OBJECTS = simpleExampleDDSImpl
simpleExampleDDSImpl_LIBS    = SimpleExampleStubs simpleMessageTypeSupportStubs acsddsncTypeSupport acsddsnc 

# <brief description of simpleExample library>
simpleExampleNCImpl_OBJECTS = simpleExampleNCImpl
simpleExampleNCImpl_LIBS    = SimpleExampleStubs acsnc 

#
# <brief description of NCBenchmarkImpl library>
NCBenchmarkImpl_OBJECTS = NCBenchmarkImpl
NCBenchmarkImpl_LIBS    =  NCBenchmarkStubs MessageTypeSupportStubs acsnc

DDSNCBenchmarkImpl_OBJECTS = DDSNCBenchmarkImpl
DDSNCBenchmarkImpl_LIBS    = NCBenchmarkStubs MessageTypeSupportStubs acsddsncTypeSupport acsddsnc

testCDBProps_OBJECTS =  testCDBProps
testCDBProps_LIBS    =  acsddsnc acscomponent CDBPropsStubs

#
# <brief description of acsddsncTypeSupport library>
acsddsncTypeSupport_OBJECTS = $(DDS_TYPE_FILES)
acsddsncTypeSupport_LIBS    = NCBenchmarkStubs SimpleExampleStubs 

#
# Python scripts
# --------------

PY_SCRIPTS_L = testDDSNCBenchmarkClient.py \
               testDDSSimpleExample.py \
               processOutput.py \
               testNCBenchmarkClient.py
#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = 
SCRIPTS_L       = testDDS testCDB TATEnvironment TATEpilogue TATPrologue


#
# Configuration Database Files
# ----------------------------
CDB_SCHEMAS = 

# 
# DDS and IDL Files and flags
# 
USE_OPENDDS=1
DDS_FILES = NCBenchmark SimpleExample #CDBProps 
IDL_FILES = NCBenchmark SimpleExample CDBProps

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
all: do_all
	mkdir -p tmp
	@echo " . . . 'all' done" 

clean : clean_all
	$(RM) tmp 1 10 20
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"


#___oOo___
