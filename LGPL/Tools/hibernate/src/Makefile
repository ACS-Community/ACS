#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
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
# "@(#) $Id: Makefile,v 1.5 2010/04/28 13:17:57 hsommer Exp $"
#
# Makefile of hibernate 
#
# who       when      what
# --------  --------  ----------------------------------------------
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# jar files to be installed
#----------------------------
# The files: 
#    ../lib/openorb-1.3.0.jar ../lib/openorb_tools-1.3.0.jar
# are part of the openORB distribution, but shall NOT be installed,
# because they would collide with JacORB
INSTALL_FILES = ../lib/hibernate-core-4.3.11.Final.jar \
	../lib/hibernate-tools-4.3.1.Final.jar \
	../lib/hibernate-jpa-2.1-api-1.0.0.Final.jar \
	../lib/hibernate-commons-annotations-4.0.5.Final.jar \
	../lib/hibernate-c3p0-4.3.11.Final.jar \
	../lib/hibernate-ehcache-4.3.11.Final.jar \
	../lib/dom4j-1.6.1.jar \
	../lib/freemarker-2.3.23.jar \
	../lib/jboss-logging-3.1.3.GA.jar \
	../lib/jboss-transaction-api_1.1_spec-1.0.1.Final.jar \

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
	@echo " . . . 'all' done"

clean : clean_all
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all
	@echo " . . . clean_dist done"

man   : do_man
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"



#___oOo___
