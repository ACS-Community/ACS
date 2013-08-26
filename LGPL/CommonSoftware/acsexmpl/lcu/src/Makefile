#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.86 2010/02/10 13:19:23 gchiozzi Exp $" 
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# dave 2003-08-26 removed acsexmplDoor.idl
# bjeram 2003-07-08 added HelloWorld example
# bjeram 2003-07-08 added acsexmplPowerSupplyDLL to acsexmplPS_COMMON
# bgustafs 2002-04-17 added acsexmplAmsSeq
# bgustafs 2002-04-09 added Fridge, Door and Building to executables
# almamgr 2002-03-01 Removed S_T generated file.
# gchiozzi 2002-01-28 Cleaned up. Was terribly out of date
# gchiozzi 2002-01-23 Updated Makefile for ACS 1.1
# almamgr 2001-09-20 Added clean_dist target
# almamgr 2001-09-20 Added ps executable.
# almamgr 2000-11-25 link also idl files from ws part and generate stubs from idl files
# almamgr 2000-11-25 don't link generated files from ws part
# almamgr 2000-07-20 created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

# Program to build
# Can be overwritten in the call

PROG = 



#
# additional include and library search paths
#USER_LIB = 

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = 	acsexmplPowerSupplyImpl \
			acsexmplMountImpl \
			acsexmplRampedPowerSupplyImpl \
			acsexmplLampImpl \
			acsexmplFridgeImpl \
			acsexmplDoorImpl \
			acsexmplBuildingImpl \
			acsexmplAmsSeqImpl \
			acsexmplHelloWorldImpl
EXECUTABLES_L   = 

#
# <brief description of xxxxx program>

acsexmplPowerSupplyImpl_COMMON    = acsexmplPowerSupplyImpl acsexmplPowerSupplyCurrentImpl acsexmplPowerSupplyDLL
acsexmplPowerSupplyImpl_GENERATED = acsexmplPowerSupplyC acsexmplPowerSupplyS
acsexmplPowerSupplyImpl_OBJECTS   = $(acsexmplPowerSupplyImpl_COMMON) $(acsexmplPowerSupplyImpl_GENERATED)

acsexmplRampedPowerSupplyImpl_COMMON    = acsexmplRampedPowerSupplyImpl
acsexmplRampedPowerSupplyImpl_GENERATED = acsexmplRampedPowerSupplyC acsexmplRampedPowerSupplyS acsexmplPowerSupplyC acsexmplPowerSupplyS
acsexmplRampedPowerSupplyImpl_OBJECTS   = $(acsexmplRampedPowerSupplyImpl_COMMON) $(acsexmplRampedPowerSupplyImpl_GENERATED)

acsexmplMountImpl_COMMON    = acsexmplMountImpl
acsexmplMountImpl_GENERATED = acsexmplMountC acsexmplMountS
acsexmplMountImpl_OBJECTS   = $(acsexmplMountImpl_COMMON) $(acsexmplMountImpl_GENERATED)

acsexmplLampImpl_COMMON    = acsexmplLampImpl
acsexmplLampImpl_GENERATED = acsexmplLampC acsexmplLampS
acsexmplLampImpl_OBJECTS   = $(acsexmplLampImpl_COMMON) $(acsexmplLampImpl_GENERATED)

acsexmplFridgeImpl_COMMON    = acsexmplFridgeImpl
acsexmplFridgeImpl_GENERATED = acsexmplFridgeC acsexmplFridgeS
acsexmplFridgeImpl_OBJECTS   = $(acsexmplFridgeImpl_COMMON) $(acsexmplFridgeImpl_GENERATED)

acsexmplDoorImpl_COMMON    = acsexmplDoorImpl
acsexmplDoorImpl_GENERATED = acsexmplBuildingC acsexmplBuildingS
acsexmplDoorImpl_OBJECTS   = $(acsexmplDoorImpl_COMMON) $(acsexmplDoorImpl_GENERATED)

acsexmplBuildingImpl_COMMON    = acsexmplBuildingImpl
acsexmplBuildingImpl_GENERATED = acsexmplBuildingC acsexmplBuildingS
acsexmplBuildingImpl_OBJECTS   = $(acsexmplBuildingImpl_COMMON) $(acsexmplBuildingImpl_GENERATED)

acsexmplAmsSeqImpl_COMMON    = acsexmplAmsSeqImpl
acsexmplAmsSeqImpl_GENERATED = acsexmplAmsSeqS acsexmplAmsSeqC
acsexmplAmsSeqImpl_OBJECTS   = $(acsexmplAmsSeqImpl_COMMON) $(acsexmplAmsSeqImpl_GENERATED)

acsexmplHelloWorldImpl_COMMON = acsexmplHelloWorldImpl
acsexmplHelloWorldImpl_GENERATED = acsexmplHelloWorldS acsexmplHelloWorldC
acsexmplHelloWorldImpl_OBJECTS =  $(acsexmplHelloWorldImpl_COMMON) $(acsexmplHelloWorldImpl_GENERATED)
#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

#
# IDL FILES
#
IDL_FILES = acsexmplPowerSupply acsexmplMount acsexmplRampedPowerSupply acsexmplLamp acsexmplFridge acsexmplBuilding acsexmplAmsSeq acsexmplHelloWorld

#
# handle links to common source files (in MODNAME/ws/src)
#
COMMON_SOURCES = \
	$(foreach exe, $(acsexmplPowerSupplyImpl_COMMON) $(acsexmplRampedPowerSupplyImpl_COMMON) $(acsexmplMountImpl_COMMON) $(acsexmplLampImpl_COMMON) $(acsexmplFridgeImpl_COMMON) $(acsexmplDoorImpl_COMMON) $(acsexmplBuildingImpl_COMMON) $(acsexmplAmsSeqImpl_COMMON) $(acsexmplHelloWorldImpl_COMMON), $(exe).cpp)


# ../idl/acsexmplFridge.midl is added (also) sepratly becausse it has midl prefix and not idl
# there is also created link ../idl/acsexmplFridge.idl which points to nowhere since ../idl/acsexmplFridge.idl does not exist
LINK_FILES= $(addprefix ../idl/, $(addsuffix .idl, $(IDL_FILES))) \
	    ../idl/acsexmplFridge.midl \
            $(COMMON_SOURCES)


#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

#
#>>>>> END OF standard rules

#
# CHOSE PLATFORM
# --------------
# Default is UNIX, for VxVorks application next line MUST BE UNCOMMENTED
MAKE_VXWORKS = on   

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
	$(RM) *~ ../include/*~ ../dbl/*~ ../idl/*~ ../*~ ../../*~ core
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#___oOo___
