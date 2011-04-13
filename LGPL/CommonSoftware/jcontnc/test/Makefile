#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) European Southern Observatory, 2009 
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
# "@(#) $Id: Makefile,v 1.20 2011/04/13 16:51:32 rtobar Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# hsommer  2009-03-02  created a new file from getTemplate and merged in the old content if it was still needed.
#


EXECUTABLES_L = archiveeventsSupplier

archiveeventsSupplier_OBJECTS = archiveeventsSupplier
archiveeventsSupplier_LIBS    = baci maciClient maciStubs maci

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         =
SCRIPTS_L       = runHelperTest.sh jcontncTATRunTest.sh runNCSubscriberTests.sh


#
# Configuration Database Files
# ----------------------------
CDB_SCHEMAS = 

# 
# IDL Files and flags
# 
IDL_FILES = adminTest1 adminTest2 adminTest3
TAO_IDLFLAGS =
USER_IDL =

#
# Jarfiles and their directories
#
JARFILES= jcontnctest
jcontnctest_DIRS=alma
#
# java sources in Jarfile on/off
DEBUG=on


#
# other files to be installed
#----------------------------
INSTALL_FILES =

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
	$(RM) *~ ref/*~
	$(RM) tatlogs
	$(RM) tmp
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"


#___oOo___
