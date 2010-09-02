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
# "@(#) $Id: Makefile,v 1.20 2010/09/02 17:00:12 hsommer Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi 2003-05-17 created
# rtobar   2008-02-19 added tools.jar file to classpath
#

#
# 3rd party Install files
#
INSTALL_FILES = ../lib/jhall-2.0_05.jar

#
# Jarfiles and their directories
#

DEBUG = on

JARFILES = jACSUtil
jACSUtil_DIRS = com alma
jACSUtil_EXTRAS = alma/acs/jhelpgen/content/*.gif

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = acsExtractJavaSources acsJarPackageInfo acsJarsearch acsJarSignInfo acsJarUnsign acsJavaHelp

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

# New CLASSPATH must use tools.jar file to compile MBeans-related classes
#os  = $(shell uname)
#ifeq ($(os),Linux)
#   SEPARATOR=:
#else ifeq ($(os),$(CYGWIN_VER))
#   SEPARATOR=;
#else
#   SEPARATOR=:
#endif
CLASSPATH := $(JAVA_HOME)/lib/tools.jar$(PATH_SEP)$(CLASSPATH)$(PATH_SEP).

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
	@echo " . . . copying jacorb property files"
	-@cp ../config/orb.properties $(ACSDATA)/config;
	@echo " . . . installation done"

#___oOo___
