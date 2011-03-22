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
# "@(#) $Id$"
#
# Makefile of extjars
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi 2003-06-05 Do not install any more openorb jars colliding with JacORB
# dvitas  08/05/03  created
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
INSTALL_FILES = ../lib/activation.jar ../lib/avalon-framework.jar ../lib/commons-cli-1.2.jar ../lib/commons-lang-2.5.jar ../lib/commons-logging-1.1.1.jar ../lib/commons-math-2.1.jar ../lib/dom4j-1.6.1.jar ../lib/ehcache-core-2.3.1.jar ../lib/gnu-regexp-1.1.4.jar ../lib/icons.jar ../lib/infobus.jar ../lib/jakarta-oro-2.0.5.jar ../lib/jakarta-regexp-1.2.jar ../lib/jaxen-1.1.3.jar ../lib/jdom.jar ../lib/jnlp.jar ../lib/junit-4.8.2.jar ../lib/logkit.jar ../lib/log4j-1.2.15.jar ../lib/openorb13Debug.jar ../lib/prevayler-1.02.001.jar ../lib/saxpath.jar ../lib/tablelayout.jar ../lib/xalan.jar ../lib/xalan_serializer.jar ../lib/commons-collections-3.2.1.jar ../lib/javassist-3.4.GA.jar ../lib/jta-1.1.jar ../lib/slf4j-api-1.5.8.jar ../lib/ejb3-persistence.jar ../lib/hsqldb.jar ../lib/sqltool.jar ../lib/jlibeps-0.1.jar ../lib/logkit.jar ../lib/xmlunit-1.3.jar

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
all:	copy_avalon_from_jacorb copy_logkit_from_jacorb do_all
	@echo " . . . 'all' done"

copy_avalon_from_jacorb :
	@echo "== Copying avalon 4.1.5 jar file used for building jacorb to this module."
	cp -a $(JACORB_HOME)/lib/avalon-framework-4.1.5.jar ../lib/avalon-framework.jar

copy_logkit_from_jacorb :
	@echo "== Copying logkit 1.2 jar file used for building jacorb to this module."
	cp -a $(JACORB_HOME)/lib/logkit-1.2.jar ../lib/logkit.jar

clean : clean_all
	@rm -f ../lib/avalon-framework.jar
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all
	@echo " . . . clean_dist done"

man   : do_man
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"



#___oOo___
