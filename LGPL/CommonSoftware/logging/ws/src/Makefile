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
#
# "@(#) $Id: Makefile,v 1.74 2012/02/20 16:22:27 acaproni Exp $" 
#
# Makefile of ........
#
# who       when        what
# --------  --------    ----------------------------------------------
# msekoran  2001-07-12  added loggingClient
# msekoran  2001-07-05  added loggingService 
# msekoran  2000-09-13  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

USER_CFLAGS = 
##
## @todo
## -lACE commented out because it makes the container crash on exit
## Seems a problem with singletons, being investigated.
##
USER_LIB =

#USER_INC =

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
EXECUTABLES     = $(PROG) loggingService loggingClient
EXECUTABLES_L   = 

loggingService_OBJECTS = loggingService loggingACSLogFactory_i \
                         loggingACSStructuredPushSupplier \
                         loggingACSStructuredPushSupplierBin \
                         loggingACSStructuredPushSupplierXml loggingACSLog_i \
                         loggingHelper  loggingAcsLogServiceImpl

loggingService_LIBS = logging logging_idlStubs TAO_CosNotification_Serv
#loggingService_LDFLAGS= -ggdb

loggingClient_OBJECTS = loggingClient loggingHelper 
loggingClient_LIBS = logging TAO_CosNotification_Serv
#loggingClient_LDFLAGS= -ggdb

# <brief description of xxxxx program>

$(PROG)_OBJECTS            = $(PROG)
$(PROG)_LDFLAGS            =
$(PROG)_LIBS               = $(TESTLIBS) C++ CCS



#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

#
# Includes (.h) files (public and local)
# ---------------------------------
INCLUDES        = 	logging.h loggingExport.h loggingBaseExport.h loggingXMLElement.h loggingXMLParser.h \
		  	loggingClient.h loggingLocalSyslog.h loggingRemoteSyslog.h loggingLocalFile.h \
			loggingCacheLogger.h \
			loggingBaseLog.h loggingHandler.h loggingLogger.h loggingLoggable.h \
                        loggingLogTrace.h loggingACSLogger.h \
			loggingLogSvcHandler.h  loggingLogLevelDefinition.h \
			loggingLoggingTSSStorage.h loggingLoggingProxy.h loggingMACROS.h \
			loggingACEMACROS.h loggingGetLogger.h loggingGenericLogger.h loggingStdoutHandler.h \
			loggingAcsLogServiceImpl.h loggingStdoutlayout.h loggingXmlLayout.h\
            loggingACSCategory.h loggingACSLoggingEvent.h loggingLogThrottle.h \
            loggingACSRemoteAppender.h loggingLog4cpp.h loggingLog4cppMACROS.h \
			loggingLog4cppACEMACROS.h loggingACSHierarchyMaintainer.h loggingThrottleAlarmInterface.h \
			loggingStopWatch.h
INCLUDES_L      = loggingService.h \
		  loggingACSStructuredPushSupplier.h \
		  loggingACSStructuredPushSupplierXml.h \
          loggingACSStructuredPushSupplierBin.h \
          loggingACSLogFactory_i.h loggingACSLog_i.h \
		  loggingHelper.h logging ACSHierarchyMaintainer.h 

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       = baselogging logging 
LIBRARIES_L     =

baselogging_OBJECTS =	loggingBaseLog loggingHandler loggingLogger \
                        loggingLogTrace loggingGetLogger loggingGenericLogger \
                        loggingStdoutHandler
##
## @todo
## -lTAO_PortableServer and acsutil commented out because it makes the container crash on exit
## Seems a problem with singletons, being investigated.
##
##baselogging_LDFLAGS =   -lTAO_PortableServer
##baselogging_LIBS =    loki acsutil
baselogging_LIBS =    acsutil loki
baselogging_LDFLAGS = -ggdb
baselogging_CFLAGS  = -DLOGGINGBASE_BUILD_DLL
logging_OBJECTS 	=	logging loggingLoggable loggingXMLElement loggingXMLParser \
						loggingLocalSyslog loggingRemoteSyslog loggingLocalFile \
						loggingLoggingTSSStorage loggingLogLevelDefinition \
						loggingLoggingProxy loggingACSLogger loggingLogSvcHandler  \
						loggingLogThrottle \
						loggingACSLoggingEvent loggingXmlLayout \
						loggingStdoutlayout loggingACSCategory \
						loggingACSRemoteAppender loggingACSHierarchyMaintainer \
						loggingLog4cpp loggingStopWatch
						
logging_LIBS    =	logging_idlStubs acsutil acscommonStubs baselogging log4cpp pthread rt
logging_LDFLAGS =	-gdb
logging_CFLAGS  = -DLOGGING_BUILD_DLL

#log4cpplogging_OBJECTS 	:= 	loggingACSLoggingEvent loggingXmlLayout \
#							loggingStdoutlayout loggingACSCategory \
#							loggingACSRemoteAppender loggingACSHierarchyMaintainer \
#							loggingLog4cpp loggingLogThrottle
#log4cpplogging_LIBS		:=	log4cpp logging_idlStubs

#
# other files to be installed
#----------------------------
INSTALL_FILES = ../idl/loggingMI.xsd

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = 
SCRIPTS_L       =

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

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#___oOo___
