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
# "@(#) $Id: Makefile,v 1.2 2009/03/03 13:22:46 hsommer Exp $"
#
# Makefile of oAW 
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
INSTALL_FILES = ../lib/org.openarchitectureware.adapter.emf.check_4.3.1.20081222-1600PRD.jar \
				../lib/org.openarchitectureware.base_4.3.1.20081222-1600PRD.jar \
				../lib/org.openarchitectureware.check.editor_4.3.1.20081222-1600PRD.jar \
				../lib/org.openarchitectureware.core.emftools_4.3.1.20081222-1600PRD.jar \
				../lib/org.openarchitectureware.core.expressions_4.3.1.20081222-1600PRD.jar \
				../lib/org.openarchitectureware.core.workflow_4.3.1.20081222-1600PRD.jar \
				../lib/org.openarchitectureware.core.xpand2_4.3.1.20081222-1600PRD.jar \
				../lib/org.openarchitectureware.dependencies_4.3.1.20081222-1600PRD.jar \
				../lib/org.openarchitectureware.emftools_4.3.1.20081222-1600PRD.jar \
				../lib/org.openarchitectureware.uml2.adapter_4.3.1.20081222-1600PRD.jar \
				../lib/org.openarchitectureware.uml2.adapter.plugin_4.3.1.20081222-1600PRD.jar \
				../lib/org.openarchitectureware.util.stdlib_4.3.1.20081222-1600PRD.jar \
				../lib/org.openarchitectureware.util.uml2ecore_4.3.1.20081222-1600PRD.jar \
				../lib/org.openarchitectureware.workflow_4.3.1.20081222-1600PRD.jar \
				../lib/org.openarchitectureware.xpand2.editor_4.3.1.20081222-1600PRD.jar \
				../lib/org.openarchitectureware.xtend.editor_4.3.1.20081222-1600PRD.jar

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
