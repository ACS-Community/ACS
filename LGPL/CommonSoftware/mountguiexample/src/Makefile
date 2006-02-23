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
# "@(#) $Id: Makefile,v 1.4 2004/09/16 09:48:40 mpasquat Exp $"
#
# Makefile of Abeans
#
# who       when      what
# --------  --------  ----------------------------------------------
# msekoran 2003-01-24 Removed OB jars
# gchiozzi 2002-11-12 Added junit jar file
# gchiozzi 2002-07-11 Ignored errors when copying/chmod abeans files.
# almamgr 2002-04-07 Fixed bug in chmod for config/abeans: 755 and not 644
# gchiozzi 2002-03-13 Added making and installing endorsed jar files.
# gchiozzi 2002-02-15 Put third party jar files in tar file.
# gchiozzi 2002-01-22 Updated version of jcchart.
# jdovc	   2002-01-22 Replaced abeansRun with abeansStart
# gchiozzi 2001-09-21 Added abeansTutorial jar file
# gchiozzi 2001-09-19 Added configuration files for abeans and clean_dist target
# almamgr 2001-09-18 uses acsMakefile and not vltMakefile
# jdovc	    17/09/01  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

#
# Jarfiles and their directories
#
JARFILES = dartboard antMountGUI 

antMountGUI_DIRS = antMountGUI 

dartboard_DIRS = dartboard
dartboard_EXTRAS = dartboard/*.gif


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
	$(RM) ../*~ ../*/*~ 
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#___oOo___
