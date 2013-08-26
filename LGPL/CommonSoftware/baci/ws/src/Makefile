#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) European Southern Observatory, 2002
# Copyright by ESO (in the framework of the ALMA collaboration)
# and Cosylab 2002, All rights reserved
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307  USA
#
# "@(#) $Id: Makefile,v 1.135 2012/10/09 14:22:54 bjeram Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# bjeram 2003-07-08 removed baciAlarm*
# gchiozzi 2003-05-29 Added missing include files
# oat 2003-01-21 added baciMonitor_T.h (templates)
# gchiozzi 2002-12-12 Removed CCS library
# gchiozzi 2002-10-17 Added CDB_SCHEMAS target.
# gchiozzi 2001-07-17 Removed IDL files from INCLUDE. Redundant.
# almamgr   2000-07-20  created
# msekoran  2001-02-19  file names changed (module prefix added)
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

USER_CFLAGS = 
USER_LIB =
USER_INC = 

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
EXECUTABLES     = $(PROG)
EXECUTABLES_L   = 

#
# <brief description of xxxxx program>

$(PROG)_OBJECTS            = $(PROG)
$(PROG)_LDFLAGS            =
$(PROG)_LIBS               = $(TESTLIBS) C++ CCS



#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

# Includes (.h) files (public and local)
# ---------------------------------

INCLUDES      = baciCharacteristicModelImpl.h baciCharacteristicComponentImpl.h \
		baciAlarm_T.h baciAlarm_T.i \
		baciPcommonImpl_T.h baciPcommonImpl_T.i baciPcontImpl_T.h baciPcontImpl_T.i \
		baciROcommonImpl_T.h baciROcommonImpl_T.i baciROcontImpl_T.h \
                baciROcontImpl_T.i \
                baciROdiscImpl_T.h baciROdiscImpl_T.i \
	        baciROSeqCommonImpl_T.h baciROSeqContImpl_T.h baciROSeqContImpl_T.i \
		baciRWcommonImpl_T.h baciRWcommonImpl_T.i baciRWcontImpl_T.h \
                baciRWcontImpl_T.i \
                baciRWdiscImpl_T.h baciRWdiscImpl_T.i \
		baciRWSeqContImpl_T.h baciRWSeqContImpl_T.i \
		baciCORBAMem.h \
		baciDevIO.h \
		baciDevIOMem.h\
		baciExport.h \
		baciValue.h baciValue.i baciThread.h baciTypes.h \
		baci.h baciTime.h \
		baciCORBA.h baciDB.h \
		baciRecovery.h baciRecoverableObject.h \
		baciEvent.h baciCDBPropertySet.h  \
		baciROdouble.h baciRWdouble.h \
		baciROfloat.h baciRWfloat.h \
		baciROlongLong.h baciRWlongLong.h \
		baciROuLongLong.h baciRWuLongLong.h \
		baciROlong.h baciRWlong.h \
		baciROuLong.h baciRWuLong.h \
		baciROboolean.h baciRWboolean.h \
		baciPpatternImpl.h baciROpattern.h baciRWpattern.h \
		baciROstring.h baciRWstring.h \
		baciROdoubleSeq.h baciROfloatSeq.h baciROlongSeq.h baciROuLongSeq.h baciRObooleanSeq.h\
		baciRWdoubleSeq.h baciRWfloatSeq.h baciRWlongSeq.h baciRWuLongSeq.h baciRWbooleanSeq.h\
		baciMonitor_T.h baciMonitor_T.i \
		baciRegistrar.h baciRegistrar.i \
		baciError.h \
		baciPropertyImpl.h\
		baciSmartPropertyPointer.h baciSmartPropertyPointer.i \
		baciSmartServantPointer.h baciSmartServantPointer.i \
		baciBACIAction.h baciBACICallback.h baciBACIMonitor.h baciUtil.h baciBACIProperty.h baciBACIComponent.h \
		baciAlarmSystemMonitorCont_T.h baciAlarmSystemMonitorCont_T.i \
		baciAlarmSystemMonitorDisc_T.h baciAlarmSystemMonitorDisc_T.i \
		baciAlarmSystemMonitorSeqCont_T.h  baciAlarmSystemMonitorSeqCont_T.i \
		baciAlarmSystemMonitorSeqDisc_T.h baciAlarmSystemMonitorSeqDisc_T.i \
		baciAlarmSystemMonitorPattern.h \
	    baciAlarmSystemMonitorBase.h    baciAlarmSystemMonitor_T.h  baciAlarmSystemMonitor_T.i


INCLUDES_L	=

#
# Libraries (public and local)
# ----------------------------
LIBRARIES = baci
LIBRARIES_L =

baci_OBJECTS =  baciValue baciBACIAction baciBACICallback baciBACIMonitor baciUtil baciBACIProperty baciBACIComponent \
		baciCharacteristicModelImpl baciCharacteristicComponentImpl\
		baciError \
		baci baciTime baciDB \
		baciRecovery baciCORBA baciDLL \
		baciEvent baciCDBPropertySet \
		baciROdouble baciRWdouble \
		baciROfloat baciRWfloat \
		baciROlongLong baciRWlongLong \
		baciROuLongLong baciRWuLongLong \
		baciROlong baciRWlong \
		baciROuLong baciRWuLong \
		baciROboolean baciRWboolean \
		baciAlarmSystemMonitorBase \
		baciPpatternImpl baciROpattern baciRWpattern baciAlarmPattern baciAlarmSystemMonitorPattern\
	        baciROstring baciRWstring \
		baciROdoubleSeq baciROfloatSeq baciROlongSeq baciROuLongSeq baciRObooleanSeq \
		baciRWdoubleSeq baciRWfloatSeq baciRWlongSeq baciRWuLongSeq baciRWbooleanSeq
baci_LIBS = baciErrTypeProperty baciErrTypeDevIO acsThread acsErrTypeLifeCycle alSysSource acsAlSysSource \
			acsErrTypeAlarmSourceFactory ACSErrTypeMonitor PatternAlarmCleared PatternAlarmTriggered \
			acsutil cdb logging acscomponent recovery acserr baciStubs archiveevents TAO_CosProperty_Serv
baci_CFLAGS = -DBACI_BUILD_DLL
 
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
CDB_SCHEMAS = 
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
IDL_FILES = 


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
	$(RM) *~ ../include/*~ ../dbl/*~ ../idl/*~ ../*~ core
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"


#___oOo___
