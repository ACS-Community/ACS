#*******************************************************************************
# ALMA - Atacama Large Millimeter Array
# Copyright (c) ESO - European Southern Observatory, 2011
# (in the framework of the ALMA collaboration).
# All rights reserved.
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#*******************************************************************************
#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id$"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# hsommer  03/05/04  created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

# Name of the jarfile which contains the generators' files
TMCDB_GRAMMAR=tmcdbGrammar
SM_GENERATOR=smGenerator


# Helper variables for extra content in jarfiles
SMGENERATOR_EXTRAFILES  = $(wildcard alma/acs/sm/profiles/*.profile.uml)
SMGENERATOR_EXTRAFILES += $(wildcard alma/acs/sm/templates/*.xpt)
SMGENERATOR_EXTRAFILES += $(wildcard alma/acs/sm/templates/*.ext)
SMGENERATOR_EXTRAFILES += $(wildcard alma/acs/sm/templates/Java/*.xpt)
SMGENERATOR_EXTRAFILES += $(wildcard alma/acs/sm/workflows/*.mwe)
SMGENERATOR_EXTRAFILES += $(wildcard alma/acs/sm/*.chk)

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         = generateTmcdbSchemas generateTmcdbHibernateStrategy generateStateMachines
SCRIPTS_L       =


#
# Jarfiles and their directories
#
JARFILES=acscodegen tmcdbGenerator
acscodegen_DIRS=alma/acs/genfw/
acscodegen_EXTRAS=
tmcdbGenerator_DIRS=alma/acs/tmcdb/translator
tmcdbGenerator_EXTRAS=alma/acs/tmcdb/generator

#
# java sources in Jarfile on/off
DEBUG=on

#
# other files to be installed
#----------------------------
INSTALL_FILES = ../lib/$(SM_GENERATOR).jar

#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach rtos, $(RTAI_MODULES) , $($(rtos)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

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

CLASSPATH = $(shell echo $CLASSPATH)$(PATH_SEP)../src-gen

# "all" includes the code generation and the jarfile with the files needed for TMCDB schemas generation
all:	.done_generating do_all
	@$(MAKE) -C ../src-merged
	@$(MAKE) ../lib/$(SM_GENERATOR).jar
	@echo " . . . 'all' done" 

# Cleaning also cleans our own-created files
clean : clean_all 
	@echo "== Deleting generated sources"
	@rm -f ../lib/$(SM_GENERATOR).jar
	@rm -rf ../src-gen
	@$(MAKE) -C ../src-merged clean
	@rm -f .done_generating
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@$(MAKE) -C ../src-merged install
	@echo " . . . installation done"

# Target for generating code from the grammar definition of the TMCDB and compile it.
.done_generating : alma/acs/tmcdb/grammardef/GenerateTmcdbTables.mwe alma/acs/tmcdb/grammardef/TmcdbTables.xtext
	@echo "== Generating classes from TMCDB grammar..."
	@cd ..; CLASSPATH="$$CLASSPATH$(PATH_SEP)./src$(PATH_SEP)./src-gen" acsStartJava org.eclipse.emf.mwe.core.WorkflowRunner alma/acs/tmcdb/grammardef/GenerateTmcdbTables.mwe
	@echo "== Merging generated and existing sources"
	@mkdir -p ../src-merged/alma/acs/
	@cp -r ../src-gen/alma/acs/tmcdb ../src-merged/alma/acs/
	@cp -r alma/acs/tmcdb/grammardef ../src-merged/alma/acs/tmcdb/
	@touch .done_generating

# the javaMakefile of ACS doesn't support the creation of jarfiles without classes.
# Therefore, we need to create them by hand.
../lib/$(TMCDB_GRAMMAR).jar :
	@echo "== Making $(TMCDB_GRAMMAR).jar"
	@$(MAKE) -C ../src-merged

../lib/$(SM_GENERATOR).jar : $(SMGENERATOR_EXTRAFILES)
	@echo "== Making $(SM_GENERATOR).jar"
	@jar cf $@ $(SMGENERATOR_EXTRAFILES)

#___oOo___
