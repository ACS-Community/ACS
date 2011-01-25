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
# "@(#) $Id: Makefile,v 1.2 2011/01/25 15:54:55 htischer Exp $"
#
# Makefile of log4cpp port to VxWorks
#
# who       when      what
# --------  --------  ----------------------------------------------
# htischer  27/10/10  created
# htischer  25/01/11  sourcecodechange/patchcreation management
#                     unified lcu and ws repository
#					  !preliminary! still building for ws on lcu tree for build test

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
# make targets:
#
# clean_dist	Bring subtree in a state suitable for version control checkin.
#				This includes also creating a new patch version in case of
#				modifications in the extracted sourcecode.
#
# all			Build. Do unpack, patch and configure if and only if necessary.
#
# clean			Prepares for a clean rebuild, but leaves configure results.
#
# patch			patch the original log4cpp package with sourcecode updates.
#				unpack when necessary.
#
# configure		autoconfigure the build to the current runtime/build environment.
#				Unpack and patch when necessary.
#
# test			Run a subset of the provided test code.
#
# printenv		Dump the Shell environment as seen inside make
#------------------------------------------------------------------------

# Version of current patched state of log4cpp package
# Version of original tgz of log4cpp package
# These must be synchronized in ../../ws/src/Makefile and ../../lcu/src/Makefile
# Moved to a common file for inclusion 
# LOG4CPP_VER = 1.0+
# LOG4CPP_TAR = 1.0

include ../../ws/src/common.Makefile

# List of files to be linked
LINK_FILES =
#

#
# user definable C-compilation flags
#USER_CFLAGS = 

#
# additional include and library search paths
#USER_INC = 
#USER_LIB = 

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     =
EXECUTABLES_L   = 

#
# <brief description of xxxxx program>
xxxxx_OBJECTS   =	
xxxxx_LDFLAGS   =
xxxxx_LIBS      =

#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

#
# Includes (.h) files (public only)
# ---------------------------------
INCLUDES        =

#
# Libraries (public and local)
# ----------------------------
LIBRARIES       =
LIBRARIES_L     =

#
# <brief description of lllll library>
lllll_OBJECTS   =

#
# Scripts (public and local)
# ----------------------------
SCRIPTS         =
SCRIPTS_L       =

#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     =
TCL_SCRIPTS_L   =

#
# <brief description of tttttt tcl-script>
tttttt_OBJECTS  =
tttttt_TCLSH    = 
tttttt_LIBS     = 

#
# TCL libraries (public and local)
# ------------------------------
TCL_LIBRARIES   =
TCL_LIBRARIES_L =

#
# <brief description of tttlll library>
tttlll_OBJECTS  = 

#
# UIF panels (public and local)
# ----------------------------
PANELS   =
PANELS_L = 

#
# IDL Files and flags
#
IDL_FILES =
IDL_TAO_FLAGS =
USER_IDL =

#
# man pages to be done
# --------------------
MANSECTIONS =
MAN1 =
MAN3 =
MAN5 =
MAN7 =
MAN8 =

#
# local man pages
# ---------------
MANl =

#
# ASCII file to be converted into Framemaker-MIF
# --------------------
ASCII_TO_MIF = 

#
# INS_ROOT files to be installed
#-------------------------------
INS_ROOT_FILES =
INS_ROOT_DIR   =

#
# other files to be installed
#----------------------------
INSTALL_FILES =

#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES = \
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

#
#>>>>> END OF standard rules

#
# CHOSE PLATFORM
# --------------
# This is a LCU Makefile, so for VxVorks applications we set:
# Todo: Temporary treat lcu like ws to test the build
#MAKE_VXWORKS = on   

#
# INCLUDE STANDARDS
# -----------------

# Todo: Temporary treat lcu like ws to test the build
#MAKEDIRTMP := $(shell searchFile include/acsMakefile)
#ifneq ($(MAKEDIRTMP),\#error\#)
#   MAKEDIR := $(MAKEDIRTMP)/include
#   include $(MAKEDIR)/acsMakefile
#endif

#
# TARGETS
# -------

# patch and configure are not repeated on calling again, therefore no separate build rule needed.
#skip default acsMakefile things
#all:	do_all
.PHONY : all
all: configure
	@echo ". . . building . . ."
	$(MAKE) -C log4cpp-$(LOG4CPP_VER) all
	@echo " . . . 'all' done" 

#skip default acsMakefile things
#clean : clean_all 
.PHONY : clean 
clean : 
	@if [ -d log4cpp-$(LOG4CPP_VER) ]; then \
		echo $(MAKE) -C log4cpp-$(LOG4CPP_VER) clean; \
		$(MAKE) -C log4cpp-$(LOG4CPP_VER) clean; \
	fi
	@echo " . . . clean done"

# Create a new patch version to reflect changes in the sourcecode.
# Before that clean up the build of the package to the state of distribution (nested distclean)
# No need to do a clean of the package, because done in turn of distclean.
# After creating a new patch, to be ready for checkin, rm also unpacked and patched subtrees
# Attention!
# Do the rm of the modified source only if the new patch version was successfully created!
# Check on folder, not on Makefile, because folder must be removed after creating a patch,
# and if Makefile vanished unintentionally, manual fix is more appropriate as just overwriting something.
# For creating the patch use the make of the ws side.
# Only if lcu side has no added files.
# For this cleanup and check whether non-symlinks remain. If yes, request manual sync with ws.
# Files deleted on lcu not checked.
# To be able to run clean_dist on ws side, run configure first on ws side. This cannot be done on
# "make patch" to not cluttering the copy of the ws files on the lcu side.
# Skipping ws side configure on ws side if Makefile does not exist too dangerous - maybe cleaned up incompletely.
# If the ws side of the package no longer exists this does no harm - on clean_dist lcu changes got checked in also.  

#skip default acsMakefile things
#clean_dist : clean_all clean_dist_all 
.PHONY : clean_dist 
clean_dist :
	@echo ". . . clean_dist . . ."
	rm -f `find .. -name "*~"`
	@if [ -d log4cpp-$(LOG4CPP_VER) ]; then \
		echo $(MAKE) -C log4cpp-$(LOG4CPP_VER) distclean; \
		$(MAKE) -C log4cpp-$(LOG4CPP_VER) distclean || exit 1; \
		if [ $$(find log4cpp-$(LOG4CPP_VER) -type f 2>&1 | wc -l) != 0 ]; then \
			echo ======== Error! ========; \
			find ../../lcu/src/log4cpp-$(LOG4CPP_VER) -type f; \
			echo files added and missing in ../../ws/src/log4cpp-$(LOG4CPP_VER); \
			echo either move to .../ws/... and place a symbolic link to them at corresponding location in .../lcu/...; \
			echo or alternatively remove.; \
			echo then run ''make configure'' and try again.; \
			exit 1; \
		fi; \
		echo $(MAKE) -C ../../ws/src configure; \
		$(MAKE) -C ../../ws/src configure; \
		echo $(MAKE) -C ../../ws/src clean_dist; \
		$(MAKE) -C ../../ws/src clean_dist; \
	fi
	rm -rf configure.log log4cpp-$(LOG4CPP_VER)
	@echo " . . . clean_dist done"

#skip default acsMakefile things
#man   : do_man 
.PHONY : man
man :
	$(MAKE) -C log4cpp-$(LOG4CPP_VER)/doc all install
	@echo " . . . man page(s) done"

# Do Nothing, for compatibility
.PHONY: db
db :
	@echo " . . . ../DB done"

#skip default acsMakefile things
#install : install_all
.PHONY : install 
install : 
	@echo ". . . installing cppunit . . ."
	$(MAKE) -C log4cpp-$(LOG4CPP_VER) install
	@echo " . . . installation done"

# Folder name of patched version of extracted tgz. Create only if not existing, therefore no dependencies.
# Link over the unpacked and patched source from the ws branch.
# Before that bring the ws branch into the state ready for version management checkin.
# Reuse the Makefile rules of the ws branch for this.
# To be able to add lcu specific files to the lcu branch during configure and build,
# do not link the entire subtree in one piece, but duplicate the folder structure
# and 'ln -s' each file individually.
# Note:
# If ws clean_dist fails, because it was left behind half way through, run configure
# on ws side or start over from scratch by manually deleting the folder, but manually
# preserving the modified source files.
# Alternatives:
# absolute  symbolic links, mkdir not in inner loop:
#   mkdir -p $$(cd ../../ws/src; find log4cpp-$(LOG4CPP_VER) -type d)
#	@STARTPWD=`pwd`; \
#	for dir in `find log4cpp-1.0+ -type d`; do \
#		echo ln -s -t $$dir `find $$STARTPWD/../../ws/src/$$dir -maxdepth 1 -type f`; \
#		ln -s -t $$dir `find $$STARTPWD/../../ws/src/$$dir -maxdepth 1 -type f`; \
#	done
# relative symbolic links, nested commands on echo not expanded, mkdir not in inner loop
#   mkdir -p $$(cd ../../ws/src; find log4cpp-$(LOG4CPP_VER) -type d)
#	@for dir in `find log4cpp-1.0+ -type d`; do \
#		echo ln -s -t $$dir \$$\(cd $$dir\; find \$$\(sed -e "s#[^/]*#..#g" \<\<\<$$dir \)/../../ws/src/$$dir -maxdepth 1 -type f\); \
#		ln -s -t $$dir $$(cd $$dir; find $$(sed -e "s#[^/]*#..#g" <<<$$dir )/../../ws/src/$$dir -maxdepth 1 -type f); \
#	done

log4cpp-$(LOG4CPP_VER) :
	$(MAKE) -C ../../ws/src clean_dist
	$(MAKE) -C ../../ws/src patch
	@for dir in $$(cd ../../ws/src; find log4cpp-1.0+ -type d); do \
		echo mkdir -p $$dir; \
		mkdir -p $$dir; \
		echo ln -s -t $$dir $$(cd $$dir; find $$(sed -e "s#[^/]*#..#g" <<<$$dir )/../../ws/src/$$dir -maxdepth 1 -type f); \
		ln -s -t $$dir $$(cd $$dir; find $$(sed -e "s#[^/]*#..#g" <<<$$dir )/../../ws/src/$$dir -maxdepth 1 -type f); \
	done

# pseudo target name is an alias of the patched folder name. Create as dependency.
# unpack and cannot be a dependency of log4cpp-$(LOG4CPP_VER) but required, therefore
# prepend here as dependency.
.NOTPARALLEL :
.PHONY : patch 
patch : log4cpp-$(LOG4CPP_VER)


# File name of one configure result. Created by configure.
# Create only if not existing, therefore no dependencies.
# Todo: Temporary treat lcu like ws to test the build
log4cpp-$(LOG4CPP_VER)/Makefile :
	@echo ". . . running configure . . ."
	echo "   log4cpp tar file is: log4cpp-$(LOG4CPP_TAR).tar.gz" >configure.log
	echo "   log4cpp version  is: $(LOG4CPP_VER)" >>configure.log
	cd log4cpp-$(LOG4CPP_VER); ./configure --enable-shared --prefix=$(INSTALL_DIR) >>../configure.log 2>&1
	@echo " . . . configuration file created"

# Do the configure only once if not done now. Use Makefile existence as indicator.
# Separate rule from patch allows to observe the patched result without configure additions,
# and also allows repeating configure in case of failed clean_dist.
# Use .NOTPARALLEL to avoid need of artifical dependencies for build order of dependencies on same line.
.NOTPARALLEL :
.PHONY : configure
configure : patch log4cpp-$(LOG4CPP_VER)/Makefile

# Debugging

.PHONY : printenv 
printenv:
	env | sort

# Usage 
# testmain, 10000 iterations.
# optional 1st argument: unknown meaning. Hangs.
# testbench, 12 iterations.
# optional 1st argument: records per iteration, default=100
# optional 2nd argument: record size, default=128
# Todo: Temporary treat lcu like ws to test the build
#
.PHONY : test 	
test :
#	log4cpp-$(LOG4CPP_VER)/tests/testmain
#	log4cpp-$(LOG4CPP_VER)/tests/testmain unknownarg
	log4cpp-$(LOG4CPP_VER)/tests/testbench 10 5
#	log4cpp-$(LOG4CPP_VER)/tests/testbench 10
#	log4cpp-$(LOG4CPP_VER)/tests/testbench

#___oOo___
