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
# "@(#) $Id: Makefile,v 1.4 2010/10/22 11:22:42 hsommer Exp $"
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
INSTALL_FILES = ../lib/com.google.collect_0.8.0.v200908120607.jar \
                   ../lib/com.ibm.icu_4.0.1.v20090822.jar \
                   ../lib/de.itemis.xtext.antlr_0.7.2.200908121408.jar \
                   ../lib/org.eclipse.emf.codegen_2.5.0.v200906151043.jar \
                   ../lib/org.eclipse.emf.common_2.5.0.v200906151043.jar \
                   ../lib/org.eclipse.emf.ecore_2.5.0.v200906151043.jar \
                   ../lib/org.eclipse.emf.mwe.core_0.7.2.v200908120417.jar \
                   ../lib/org.eclipse.uml2.uml_3.0.1.v200908281330.jar \
                   ../lib/org.eclipse.xpand_0.7.2.v200908120436.jar \
                   ../lib/org.eclipse.xtend_0.7.2.v200908120436.jar \
                   ../lib/org.eclipse.xtend.typesystem.emf_0.7.2.v200908120436.jar \
                   ../lib/org.eclipse.xtend.typesystem.uml2_0.7.2.v200908120436.jar \
                   ../lib/org.eclipse.xtend.util.stdlib_0.7.2.v200908120436.jar \
                   ../lib/org.eclipse.xtext_0.7.2.v200908120607.jar \
                   ../lib/org.eclipse.xtext.generator_0.7.2.v200908120607.jar \
                   ../lib/org.eclipse.xtext.util_0.7.2.v200908120607.jar \
                   ../lib/org.eclipse.xtext.xtend_0.7.2.v200908120607.jar \
                   ../lib/org.eclipse.text_3.5.0.v20090513-2000.jar \
                   ../lib/org.eclipse.emf.mwe.utils_0.7.2.v200908120417.jar \
                   ../lib/org.eclipse.emf.ecore.xmi_2.5.0.v200906151043.jar \
                   ../lib/org.eclipse.emf.mapping.ecore2xml_2.5.0.v200906151043.jar \
                   ../lib/org.eclipse.uml2.uml.resources_3.0.0.v200906011111.jar \
                   ../lib/org.eclipse.uml2.common_1.5.0.v200905041045.jar \
                   ../lib/org.eclipse.equinox.common_3.5.1.R35x_v20090807-1100.jar \
                   ../lib/org.eclipse.jdt.core_3.5.1.v_972_R35x.jar \
                   ../lib/org.eclipse.emf.codegen.ecore_2.5.0.v200906151043.jar \
                   ../lib/guice-1.0_patched.jar \
                   ../lib/stringtemplate-3.1b1.jar 

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
