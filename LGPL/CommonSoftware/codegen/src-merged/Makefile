#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.2 2010/04/29 16:21:28 hsommer Exp $"
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

# Helper variables for extra content in jarfiles
TMCDBGRAMMAR_EXTRAFILES = $(wildcard alma/acs/tmcdb/grammardef/validation/*.chk)
TMCDBGRAMMAR_EXTRAFILES+= $(wildcard alma/acs/tmcdb/grammardef/*.ext)
TMCDBGRAMMAR_EXTRAFILES+= $(wildcard alma/acs/tmcdb/grammardef/*.xtext)
TMCDBGRAMMAR_EXTRAFILES+= $(wildcard alma/acs/tmcdb/grammardef/*.mwe)
TMCDBGRAMMAR_EXTRAFILES+= $(wildcard alma/acs/tmcdb/grammardef/*.properties)
TMCDBGRAMMAR_EXTRAFILES+= $(wildcard alma/acs/tmcdb/grammardef/*.ecore)
TMCDBGRAMMAR_EXTRAFILES+= $(wildcard alma/acs/tmcdb/grammardef/*.genmodel)
TMCDBGRAMMAR_EXTRAFILES+= $(wildcard alma/acs/tmcdb/grammardef/*.xmi)
TMCDBGRAMMAR_EXTRAFILES+= $(wildcard alma/acs/tmcdb/grammardef/parser/antlr/internal/*.g)
TMCDBGRAMMAR_EXTRAFILES+= $(wildcard alma/acs/tmcdb/grammardef/parser/antlr/internal/*.tokens)


#
# Scripts (public and local)
# ----------------------------
SCRIPTS         =
SCRIPTS_L       =

#
# Jarfiles and their directories
#
JARFILES=tmcdbGrammar
tmcdbGrammar_DIRS=alma/acs/tmcdb
tmcdbGrammar_EXTRAS=$(TMCDBGRAMMAR_EXTRAFILES)

#
# java sources in Jarfile on/off
DEBUG=on

#
# other files to be installed
#----------------------------
INSTALL_FILES =

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

# "all" includes the code generation and the jarfile with the files needed for TMCDB schemas generation
all:	do_all
	@echo " . . . 'all' done" 

# Cleaning also cleans our own-created files
clean : clean_all 
	rm -rf alma/acs/tmcdb/*
	@echo " . . . clean done"

clean_dist : clean_all clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#___oOo___
