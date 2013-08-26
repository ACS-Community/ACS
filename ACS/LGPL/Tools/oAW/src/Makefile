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
# "@(#) $Id: Makefile,v 1.5 2010/11/03 09:27:20 rtobar Exp $"
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
INSTALL_FILES = \
         ../lib/antlr-generator-3.0.1.jar \
         ../lib/com.google.collect_0.8.0.v201008251220.jar \
         ../lib/com.google.inject_2.0.0.v201003051000.jar \
         ../lib/com.ibm.icu_4.2.1.v20100412.jar \
         ../lib/org.antlr.runtime_3.0.0.v200803061811.jar \
         ../lib/org.eclipse.emf.codegen_2.6.0.v20100914-1218.jar \
         ../lib/org.eclipse.emf.codegen.ecore_2.6.1.v20100914-1218.jar \
         ../lib/org.eclipse.emf.common_2.6.0.v20100914-1218.jar \
         ../lib/org.eclipse.emf.ecore_2.6.1.v20100914-1218.jar \
         ../lib/org.eclipse.emf.ecore.xmi_2.5.0.v20100521-1846.jar \
         ../lib/org.eclipse.emf.mapping.ecore2xml_2.5.0.v20100521-1847.jar \
         ../lib/org.eclipse.emf.mwe2.runtime_1.0.1.v201008251113.jar \
         ../lib/org.eclipse.emf.mwe.core_1.0.0.v201008251122.jar \
         ../lib/org.eclipse.emf.mwe.utils_1.0.0.v201008251122.jar \
         ../lib/org.eclipse.equinox.common_3.6.0.v20100503.jar \
         ../lib/org.eclipse.jdt.core_3.6.1.v_A68_R36x.jar \
         ../lib/org.eclipse.text_3.5.0.v20100601-1300.jar \
         ../lib/org.eclipse.uml2.common_1.5.0.v201005031530.jar \
         ../lib/org.eclipse.uml2.uml_3.1.1.v201008191505.jar \
         ../lib/org.eclipse.uml2.uml.resources_3.1.1.v201008191505.jar \
         ../lib/org.eclipse.xpand_1.0.1.v201008251147.jar \
         ../lib/org.eclipse.xtend_1.0.1.v201008251147.jar \
         ../lib/org.eclipse.xtend.typesystem.emf_1.0.1.v201008251147.jar \
         ../lib/org.eclipse.xtend.typesystem.uml2_1.0.1.v201008251147.jar \
         ../lib/org.eclipse.xtend.util.stdlib_1.0.1.v201008251147.jar \
         ../lib/org.eclipse.xtext_1.0.1.v201008251220.jar \
         ../lib/org.eclipse.xtext.generator_1.0.1.v201008251220.jar \
         ../lib/org.eclipse.xtext.util_1.0.1.v201008251220.jar \
         ../lib/org.eclipse.xtext.xtend_1.0.1.v201008251220.jar


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
