#*******************************************************************************
# "@(#) $Id: Makefile,v 1.67 2012/01/21 22:48:11 tstaig Exp $"
#
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2002  
# (c) European Southern Observatory, 2002
# Copyright by ESO (in the framework of the ALMA collaboration),
# All rights reserved
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
# Makefile for acsnc/ws/test applications

USER_CFLAGS =
USER_LIB =

PROG = 

DEBUG=on
# C programs (public and local)
# -----------------------------
EXECUTABLES     = testChannelCreation 
EXECUTABLES_L   = 

testChannelCreation_OBJECTS =	testChannelCreation
testChannelCreation_LIBS    =	acsnc maciClient acsncHelperTest


SCRIPTS_L       = acsncTATRunTest.sh

PY_SCRIPTS_L =	acsncTestConSup

# Includes (.h) files (public and local)
# ---------------------------------
INCLUDES        = 
INCLUDES_L      = acsncConsumerCompImpl.h acsncSupplierCompImpl.h

# Libraries (public and local)
# ----------------------------
LIBRARIES       = acsncConsumerCompImpl acsncSupplierCompImpl acsncRTSupplierCompImpl \
		acsncArchiveConsumerCompImpl \
		testCDBProps  \
		acsncConsumerILCompImpl acsncSupplierILCompImpl \
        acsncHelperTest
LIBRARIES_L     = 

acsncHelperTest_OBJECTS = acsncHelperTest
acsncHelperTest_LIBS = acsnc

testCDBProps_OBJECTS =	testCDBProps
testCDBProps_LIBS    =	EventComponentStubs acsnc acscomponent


acsncConsumerCompImpl_OBJECTS = acsncConsumerCompImpl
acsncConsumerCompImpl_LIBS    = EventComponentStubs acsnc acscomponent

acsncConsumerILCompImpl_OBJECTS = acsncConsumerILCompImpl
acsncConsumerILCompImpl_LIBS    = EventComponentStubs acsnc acscomponent

acsncArchiveConsumerCompImpl_OBJECTS = acsncArchiveConsumerCompImpl
acsncArchiveConsumerCompImpl_LIBS    = EventComponentStubs acsnc acscomponent

acsncSupplierCompImpl_OBJECTS = acsncSupplierCompImpl
acsncSupplierCompImpl_LIBS    = EventComponentStubs acsnc acscomponent

acsncSupplierILCompImpl_OBJECTS = acsncSupplierILCompImpl
acsncSupplierILCompImpl_LIBS    = EventComponentStubs acsnc acscomponent

acsncRTSupplierCompImpl_OBJECTS = acsncRTSupplierCompImpl
acsncRTSupplierCompImpl_LIBS    = EventComponentStubs acsnc acscomponent


IDL_FILES = EventComponent
EventComponentStubs_LIBS = acscomponentStubs

# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))


#>>>>> END OF standard rules

#
# CHOSE PLATFORM
# --------------
# Default is UNIX, for VxVorks application next line MUST BE UNCOMMENTED
#MAKE_VXWORKS = on   

# INCLUDE STANDARDS
# -----------------
MAKEDIRTMP := $(shell searchFile include/acsMakefile)
ifneq ($(MAKEDIRTMP),\#error\#)
   MAKEDIR := $(MAKEDIRTMP)/include
   include $(MAKEDIR)/acsMakefile
endif


# TARGETS
# -------
all:	do_all
	@mkdir -p tmp
	@echo " . . . 'all' done" 

clean : clean_all 
	$(RM) *~ ref/*~
	$(RM) sed.scan
	$(RM) tmp tatlogs
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"
