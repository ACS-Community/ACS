#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2002 
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
# "@(#) $Id: Makefile,v 1.44 2012/01/24 01:00:04 tstaig Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi  11/02/02  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

PY_SCRIPTS_L =	test_Acspy_Nc_CommonNC \
		acspyTestUnitTimeHelper \
		acspyTestUnitCDBAccess \
		acspyTestUnitACSHandler \
		acspyTestACSComponent \
		acspyTestAcsCORBA \
		acspyTestBaci \
		acspyTestCallbacks \
		acspyTestCDB \
		acspyTestClientMinimum \
		acspyTestComponentLifecycle \
		acspyTestContainer \
		acspyTestContainerServices \
		acspyTestError \
		acspyTestLogging \
		acspyTestNameTree \
		acspyTestTimehelper \
		acspyTestEpochHelper \
		acspyTestDurationHelper \
		acspyTestAutoload \
		acspyTestArchiveConsumer \
		acspyTestArchiveSupplier \
		acspyTestArchiveConsumerPS \
		acspyTestCompReload \
		acspyTestLoggingConsumer \
		acspyTestSupplier acspyTestConsumer acspyTestQoSTimeout

PY_PACKAGES = acspytestImpl acspyTestQoSImpl

PY_PACKAGES_L = 

SCRIPTS_L = acspyTATPrologue


#
# other files to be installed
#----------------------------
INSTALL_FILES =

IDL_FILES = acspytest acspyTestQoS
acspytestStubs_LIBS = baciStubs
acspyTestQoSStubs_LIBS = acscomponentStubs

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
	$(RM) *~ acspytestImpl/*~ acspyTestQoSImpl/*~ UnitTests/*~
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
