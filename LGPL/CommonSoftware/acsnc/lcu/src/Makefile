#*************************************************************************
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
# "@(#) $Id: Makefile,v 1.40 2009/12/17 09:44:58 bjeram Exp $"
#
# Makefile for base class push structured event supplier & consumer
#    None
#------------------------------------------------------------------------
LINK_FILES = acsncCDBProperties.cpp acsncArchiveConsumer.cpp acsncConsumerImpl.cpp acsncSupplierImpl.cpp acsncHelperImpl.cpp acsncORBHelperImpl.cpp acsncSimpleSupplierImpl.cpp acsncRTSupplierImpl.cpp acsncSimpleConsumerImpl.cpp acsncServiceChecker.cpp acsncCircularQueue.cpp acsncReconnectionCallback.cpp


#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = $(PROG) acsnc
EXECUTABLES_L   = 

#
# acsnc is the Notification Channel server side base library
# acsncC is the NC client side base library
acsnc_OBJECTS = acsncCDBProperties \
		acsncArchiveConsumer \
		acsncConsumerImpl acsncSimpleConsumerImpl \
		acsncSupplierImpl acsncSimpleSupplierImpl acsncRTSupplierImpl \
		acsncHelperImpl acsncORBHelperImpl \
		acsncServiceChecker \
		acsncReconnectionCallback acsncCircularQueue


$(PROG)_OBJECTS            = $(PROG)
$(PROG)_LDFLAGS            =
$(PROG)_LIBS               = C++ ACE TAO TAO_CosNaming TAO_PortableServer TAO_CosTrading TAO_Svc_Utils

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
# CHOSE PLATFORM
# --------------
# This is a LCU Makefile, so for VxVorks applications we set:
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
all:	do_links do_all
	@echo " . . . 'all' done" 

clean : clean_links clean_all 
	$(RM) ../idl/acsnc.idl
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

