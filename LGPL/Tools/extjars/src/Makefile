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
INSTALL_FILES = ../lib/activation.jar ../lib/avalon-framework.jar ../lib/backport-util-concurrent.jar ../lib/commons-cli-1.0.jar ../lib/commons-lang-2.1.jar ../lib/commons-logging.jar ../lib/cosyframework-gui.jar ../lib/cosyicons.jar ../lib/cosyicons-1.0.jar ../lib/cosyframework-core.jar ../lib/genfw.jar ../lib/genfwutil.jar ../lib/gnu-regexp-1.1.4.jar ../lib/icons.jar ../lib/infobus.jar ../lib/jakarta-oro-2.0.5.jar ../lib/jakarta-regexp-1.2.jar ../lib/jaxen-core.jar ../lib/jaxen-jdom.jar ../lib/jdom.jar ../lib/jnlp.jar ../lib/junit.jar ../lib/logkit.jar ../lib/log4j-1.2.8.jar ../lib/metacore.jar ../lib/openorb13Debug.jar ../lib/prevayler-1.02.001.jar ../lib/saxpath.jar ../lib/tablelayout.jar ../lib/xalan.jar ../lib/xalan_serializer.jar ../lib/endorsed/xmlParserAPIs.jar 


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
