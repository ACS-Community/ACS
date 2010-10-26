#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: Makefile,v 1.140 2010/10/26 20:50:49 javarias Exp $" 
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# almamgr 2003-09-02 Added TAO_CosProperty library needed on Sun to link
# dfugate  2003-08-26 Removed old dbl classes.
# dfugate  2003-08-26 Removed acsexmplDoor.idl (it's in building now)
# gchiozzi 2003-05-25 Renamed in acsabeans.sh removing .sh
# rgeorgie 2002-12-05 Commented out CCS libraries
# gchiozzi 2002-10-21 Added CDB_SCHEMAS
# david 2002-08-08 removed acsexmplDoorDefines.h from INCLUDES
# david 2002-08-06 removed "-rf" from the clean abeans entry as it is not necessary
# david 2002-07-30 made doxygenize a local script so it doesn't install into ACSROOT
# gchiozzi 2002-07-18 Added first simple target to generate abeans.
# msekoran 2002-07-12 added Calendar example
# david 2002-06-20 changed to remove doxygen documentation
# david 2002-06-14 Changed gman target to man
# blopez   2002-04-04 Files for example device "Building" added
# blopez   2002-04-04 Library for ACSDO usage added
# blopez   2002-03-25 Files for example device "Door" added
# gchiozzi 2002-03-08 Added GetAsync test client
# gchiozzi 2002-02-13 Refactored example clients
# almamgr 2002-01-22 Cleaned up USER_IDL. Takes default from acsMakefile.
# almamgr 2002-01-22 Replaced old libraries with new axsexmpl... files
# gchiozzi 2002-01-21 Replaced PowerSupplyESO with PowerSupplyACS
# gchiozzi 2002-01-17 Removed all -I directives for ACE/TAO include files. Are handled by acsMakefile
# almamgr 2000-07-20 created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

USER_CFLAGS =
USER_LIB = 	-lACE \
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
                -lACSErrTypeCommon

#
# This will trigger debugging information for Java
# and generation of documentation for generated code.
#
DEBUG=on


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
EXECUTABLES     = $(PROG) \
                  acsexmplClient \
                  acsexmplAsyncCalls \
		  acsexmplClientDynamicComponent \
                  acsexmplClientWave \
                  acsexmplClientAlarmThread \
                  acsexmplClientListComponents \
                  acsexmplClientComponentIOR \
                  acsexmplClientFridgeCmd acsexmplClientFridge \
		  acsexmplClientAmsSeq \
		  acsexmplClientHelloWorld \
		  acsexmplClientHelloWorldSP \
		  acsexmplClientFridgeNC \
                  acsexmplClientErrorComponent

EXECUTABLES_L   = 

# ---> CDB_Test in text directory


#
# <brief description of xxxxx program>

acsexmplClient_OBJECTS	   = acsexmplClient
acsexmplClient_LIBS	   = acsexmplMountStubs acsexmplCallbacksImpl

acsexmplAsyncCalls_OBJECTS	= acsexmplAsyncCalls acsexmplAsyncCallbacks acsexmplAsyncMethodCB
acsexmplAsyncCalls_LIBS	   	= acsexmplMountStubs 

acsexmplClientWave_OBJECTS	   = acsexmplClientWave
acsexmplClientWave_LIBS	           = acsexmplMountStubs acsexmplCallbacksImpl

acsexmplClientAlarmThread_OBJECTS  = acsexmplClientAlarmThread
acsexmplClientAlarmThread_LIBS	   = acsexmplPowerSupplyStubs acsexmplCallbacksImpl

acsexmplClientFridgeCmd_OBJECTS	   = acsexmplClientFridgeCmd
acsexmplClientFridgeCmd_LIBS	   = acsexmplFridgeStubs acsnc

acsexmplClientFridge_OBJECTS	   = acsexmplClientFridge
acsexmplClientFridge_LIBS	   = acsexmplFridgeStubs acsnc acsexmplCallbacksImpl

acsexmplClientComponentIOR_OBJECTS 	   = acsexmplClientComponentIOR

acsexmplClientListComponents_OBJECTS           = acsexmplClientListComponents

acsexmplClientAmsSeq_OBJECTS	   = acsexmplClientAmsSeq
acsexmplClientAmsSeq_LIBS	   = acsexmplAmsSeqStubs

acsexmplClientHelloWorld_OBJECTS   = acsexmplClientHelloWorld
acsexmplClientHelloWorld_LIBS      = acsexmplHelloWorldStubs ACSErrTypeCommon

acsexmplClientHelloWorldSP_OBJECTS   = acsexmplClientHelloWorldSP
acsexmplClientHelloWorldSP_LIBS      = acsexmplHelloWorldStubs ACSErrTypeCommon

acsexmplClientFridgeNC_OBJECTS     = acsexmplClientFridgeNC
acsexmplClientFridgeNC_LIBS        = acsnc acsexmplFridgeStubs ACSErrTypeCommon


acsexmplClientDynamicComponent_OBJECTS = acsexmplClientDynamicComponent
acsexmplClientDynamicComponent_LIBS    = acsexmplHelloWorldStubs ACSErrTypeCommon

acsexmplClientErrorComponent_OBJECTS   = acsexmplClientErrorComponent
acsexmplClientErrorComponent_LIBS      = acsexmplErrorComponentStubs ACSErrTypeCommon

acsexmplCDB_OBJECTS = acsexmplCDB
acsexmplCDB_LIBS = 

acsexmplRampedPowerSupplyStubs_LIBS := acsexmplPowerSupplyStubs 

$(PROG)_OBJECTS            = $(PROG)
$(PROG)_LDFLAGS            =
$(PROG)_LIBS               = C++

#
# Includes (.h) files (public and local)
# ---------------------------------
INCLUDES =	acsexmplMountImpl.h \
		acsexmplPowerSupplyImpl.h \
		acsexmplExport.h \
		acsexmplRampedPowerSupplyImpl.h \
		acsexmplLampImpl.h \
		acsexmplFridgeImpl.h \
		acsexmplDoorImpl.h \
		acsexmplBuildingImpl.h \
		acsexmplAmsSeqImpl.h \
		acsexmplCalendarImpl.h \
		acsexmplHelloWorldImpl.h \
		acsexmplLongDevIO.h \
		acsexmplCallbacks.h \
		acsexmplAsyncCallbacks.h \
		acsexmplSlowMountImpl.h \
		acsexmplFilterWheelImpl.h \
		acsexmplErrorComponentImpl.h \
		acsexmplInitErrorHelloWorld.h \
		acsexmplConstrErrorHelloWorld.h

INCLUDES_L      = 
#
# Libraries (public and local)
# ----------------------------
LIBRARIES =	acsexmplCallbacksImpl \
		acsexmplHelloWorldImpl \
		acsexmplPowerSupplyImpl \
		acsexmplRampedPowerSupplyImpl \
		acsexmplMountImpl \
		acsexmplLampImpl \
		acsexmplFridgeImpl \
		acsexmplDoorImpl \
		acsexmplBuildingImpl \
		acsexmplAmsSeqImpl \
		acsexmplCalendarImpl \
		acsexmplSlowMountImpl \
		acsexmplLampWheelImpl \
		acsexmplFilterWheelImpl \
		acsexmplErrorComponentImpl \
		acsexmplInitErrorHelloWorld \
		acsexmplConstrErrorHelloWorld

LIBRARIES_L     =

acsexmplCallbacksImpl_OBJECTS   = acsexmplCallbacksImpl
acsexmplCallbacksImpl_LIBS	= 

acsexmplPowerSupplyImpl_OBJECTS = acsexmplPowerSupplyImpl acsexmplPowerSupplyCurrentImpl acsexmplPowerSupplyDLL 
acsexmplPowerSupplyImpl_LIBS	= acsexmplPowerSupplyStubs 

acsexmplRampedPowerSupplyImpl_OBJECTS	= acsexmplRampedPowerSupplyImpl \
	  	              acsexmplPowerSupplyImpl acsexmplPowerSupplyCurrentImpl 
acsexmplRampedPowerSupplyImpl_LIBS	= acsexmplRampedPowerSupplyStubs acsexmplPowerSupplyStubs

acsexmplMountImpl_OBJECTS	= acsexmplMountImpl
acsexmplMountImpl_LIBS		= acsexmplMountStubs

acsexmplSlowMountImpl_OBJECTS	= acsexmplSlowMountImpl
acsexmplSlowMountImpl_LIBS	= acsexmplMountStubs

acsexmplLampImpl_OBJECTS 	= acsexmplLampImpl
acsexmplLampImpl_LIBS 		= acsexmplLampStubs

acsexmplFridgeImpl_OBJECTS	= acsexmplFridgeImpl
acsexmplFridgeImpl_LIBS	   	= acsnc acsexmplFridgeStubs

acsexmplDoorImpl_OBJECTS       	= acsexmplDoorImpl
acsexmplDoorImpl_LIBS     	= acsexmplBuildingStubs

acsexmplBuildingImpl_OBJECTS   	= acsexmplBuildingImpl
acsexmplBuildingImpl_LIBS   	= acsexmplBuildingStubs

acsexmplAmsSeqImpl_OBJECTS     	= acsexmplAmsSeqImpl
acsexmplAmsSeqImpl_LIBS     	= acsexmplAmsSeqStubs

acsexmplCalendarImpl_OBJECTS   	= acsexmplCalendarImpl
acsexmplCalendarImpl_LIBS   	= acsexmplCalendarStubs

acsexmplHelloWorldImpl_OBJECTS 	= acsexmplHelloWorldImpl
acsexmplHelloWorldImpl_LIBS	= acsexmplHelloWorldStubs ACSErrTypeCommon

acsexmplLampWheelImpl_OBJECTS 	= acsexmplLampWheelImpl
acsexmplLampWheelImpl_LIBS	= acsexmplLampWheelStubs expat

acsexmplFilterWheelImpl_OBJECTS = acsexmplFilterWheelImpl
acsexmplFilterWheelImpl_LIBS = acsexmplFilterWheelStubs 

acsexmplErrorComponentImpl_OBJECTS 	= acsexmplErrorComponentImpl
acsexmplErrorComponentImpl_LIBS	= acsexmplErrorComponentStubs ACSErrTypeCommon

acsexmplInitErrorHelloWorld_OBJECTS 	= acsexmplInitErrorHelloWorld
acsexmplInitErrorHelloWorld_LIBS	= acsexmplHelloWorldStubs ACSErrTypeCommon acsexmplErrTest

acsexmplConstrErrorHelloWorld_OBJECTS 	= acsexmplConstrErrorHelloWorld
acsexmplConstrErrorHelloWorld_LIBS	= acsexmplHelloWorldStubs ACSErrTypeCommon acsexmplErrTest

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = acsexmplPSPanel
SCRIPTS_L       = 

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     =
TCL_SCRIPTS_L   =

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
# On-Line Database Files
# ----------------------
CDB_SCHEMAS = AmsSeq  Calendar Fridge \
              LAMP LAMPWHEEL MOUNT \
              PowerSupply RampedPowerSupply \
              Building Door Tower \
              FILTERWHEEL 
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
IDL_FILES = 	acsexmplPowerSupply \
		acsexmplMount \
		acsexmplRampedPowerSupply \
		acsexmplLamp \
		acsexmplFridge \
		acsexmplBuilding \
		acsexmplAmsSeq \
		acsexmplCalendar \
		acsexmplHelloWorld \
		acsexmplLampWheel \
		acsexmplFilterWheel \
                acsexmplErrorComponent
USER_IDL = 

ACSERRDEF = 	acsexmplErrTest

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
all:	do_all
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) *~ ../include/*~ ../idl/*~ ../*~ ../../*~ core
	$(RM) ../doc/html
	$(RM) tmp.txt acsexmplbeans.jar
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	cp ../doc/html/group__ACSEXMPLDOC.html ../doc/html/main.html
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

checkCdbSchemas:
	cdbChecker -v

#___oOo___
