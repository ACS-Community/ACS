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
# "@(#) $Id: Makefile,v 1.4 2011/01/31 15:38:12 htischer Exp $"
#
# Makefile of log4cpp port to VxWorks
#
# who       when      what
# --------  --------  ----------------------------------------------
# htischer  27/10/10  created
# htischer  25/01/11  sourcecodechange/patchcreation management
#                     unified lcu and ws repository
#					  !preliminary! still building for ws on lcu tree for build test
# htischer  28/01/11  configure compile and create library for VxWorks (not yet linking),
#					  install library, headers and doxygen documentation
#					  build stabilizing.
#					  exclude autom4te.cache from clean_dist.
# htischer  31/01/11  moved INSTALL_DIR to common file
#					  add compilation of testcases  

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
# make options:
# DEBUG=on		for configure, activate debugging
# make targets:
#
# all			Build. Do unpack, patch and configure if and only if necessary.
#
# clean_dist	Bring subtree in a state suitable for version control checkin.
#				This includes also creating a new patch version in case of
#				modifications in the extracted sourcecode.
#
# clean_configure remove configure results, if they got created.
#				Avoids doing a configure just to be able to do a distclean.
#				Useful to start configure from scratch or checking current state
#				before creating a new patch. 
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

# Settings to be synchronized in ../../ws/src/Makefile and ../../lcu/src/Makefile
# Moved to a common file for inclusion 

# Version of current patched state of log4cpp package
# Version of original tgz of log4cpp package
# LOG4CPP_VER = 1.0+
# LOG4CPP_TAR = 1.0

# Installation location for 'make install'
# INSTALL_DIR = e.g. $(INTROOT)

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

ifeq ($(DEBUG),)
CONFIGURE_DEBUG=
else
CONFIGURE_DEBUG=--enable-debug
endif


# Workbench VxWorks Kernel Module Project options. Use for now instead of VLT/APEX options.
#
# For VLT/APEX compilation in particular add -mlongcall
#
# options for all link steps are expecting invokation through the ccppc compiler/linker driver, not directly ldppc
# For loadable kernel modules,
# First in the "intermediate link" step all module object files are linked together intermediately to a single object file,
# then in the "munch" step a ctdt.c file gets created which covers invocation of file level C++ constructors/destructors,
# then in the "ctdt" step, ctdt.c gets compiled with some special options,
# then in the "final link" step, the intermediate object file and ctdt object get linked to the final loadable kernel module.
# The "munch" step by Workbench VxWorks Kernel Module Project is done as follows:
# nmppc $(infile) | tclsh $(WIND_BASE)/host/resource/hutils/tcl/munch.tcl -c ppc > $(outfile)
# VLT and APEX builds are combining all 4 link steps to a single aequivalent procedure.
# VLT and APEX do this differently:
# VLT creates a matching nested make file on the fly
# APEX calls a fixed pearl script with the necessary arguments to do all the steps
#
# Attention!
# configure as of log4cpp unfortunately finds out the linker executable and calls it directly
# without a chance to replacing it by something else.
# Therefore in VxWorks the log4cpp autoconfigure can be only used for compilation, and linking must
# be done separately.
# Details:
# configure uses an auto-generated ./libtool for compiling and linking which cannot be replaced.
# For linking libtool gets passed the tool name $(CXXLD). CXXLD gets defined by */Makefile.in
# */Makefile.in by autoconfigure is defined to be the compiler linker driver CXX=ccppc
# autoconf result is part of the log4cpp package and cannot be changed because automake 1.10 etc 
# is missing ALMA workstations. 
#
# Attention!
# Since all modules are loaded into the single kernel process, libraries must be loaded only one time.
# Otherwise it is not consistent which applications are using what instance of
# static data and possibly code of different versions.
# Either factor "shared" libraries out to a separate loadable kernel module, which gets loaded once,
# or link all applications which have common libraries together to one a single loadable kernel module.
#
# Note:
# For build diagnostics consider compiler options
# -v     shows effective and intended include path and compiler options with implicite arguments
#        and compiler version with integrated patches
# -H     shows inclusion order of all used header files
# -E -dM on empty file dumps predefined Macros

# Compiling for Debugging or optimization for normal files and ctdt file
# See configure.in: depending on --enable-debug, GCC_CFLAGS -g/-O added automatically
# In addition, configure adds -D_DEBUG/-DNDEBUG
#CFLAGS_OPTIMIZE+=-O2
#CFLAGS_OPTIMIZE+=-g
ifeq ($(DEBUG),)
CFLAGS_OPTIMIZE=-fstrength-reduce -fno-builtin
else
CFLAGS_OPTIMIZE=
endif

# Common for all compilations, normal files and ctdt file
# configure adds-ansi -MD -MP -Wall automatically. In addition it adds -Wno-unused -pedantic
#CFLAGS_COMMON+=-Wall -ansi -MD -MP
CFLAGS_COMMON=-mcpu=604 -mstrict-align -fno-implicit-fp \
-I$(WIND_BASE)/target/h -I$(WIND_BASE)/target/h/wrn/coreip \
-DCPU=PPC604 -DTOOL_FAMILY=gnu -DTOOL=gnu -D_WRS_KERNEL \
-D_VSB_CONFIG_FILE=\"$(WIND_BASE)/target/lib/h/config/vsbConfig.h\"

# compilation of ctdt file, C++ file level constructor/destructor list after the "munch" step
# This is not covered by configure, therefore need to add those flags manually which normally
# would be added by configure. But -ansi -MD -MP not needed for ctdt. Only add -Wall
# CFLAGS_CTDT=$(filter-out -ansi -MD -MP,$(CFLAGS_CTDT))
ifeq ($(DEBUG),)
CFLAGS_OPTIMIZE_CTDT=-O2
else
CFLAGS_OPTIMIZE_CTDT=-g
endif
CFLAGS_CTDT=$(CFLAGS_OPTIMIZE_CTDT) $(CFLAGS_OPTIMIZE) $(CFLAGS_COMMON) -fdollars-in-identifiers -Wall

# only for intermediate link for reuse result in later link step
CFLAGS_LINKPARTIAL=

# only for final link of downloable kernel module, picking up intermediate and munch results
CFLAGS_LINKFINAL=-T $(WIND_BASE)/target/h/tool/gnu/ldscripts/link.OUT

# common for both link steps 
CFLAGS_LINK=-r -nostdlib -Wl,-X

# -----

# Final composition of options and commands for configure

# C/C++ compiler options
CFLAGS=$(CFLAGS_OPTIMIZE) $(CFLAGS_COMMON)
CXXFLAGS=$(CFLAGS)

#ccppc routes the request through to c++ppc when necessary by options or file type.
#By convention with C++, VxWorks RTP and VLT/APEX for VxWorks Kernel are using c++ppc,
#and Wind River builds for VxWorks Kernel are using ccppc
#For now follow the Wind River VxWorks Kernel convention 
#CXX=c++ppc
CXX=ccppc
CC=ccppc

# Same Preprocessor for C and C++, behaviour adjustment by options and file type.
CPP=cppppc
CXXCPP=cppppc

AR=arppc
RANLIB=ranlibppc
STRIP=stripppc

#Not yet needed, but set to be correct in the auto-generated ./libtool
#DLLTOOL not available for kernel modules in Vxworks
DLLTOOL=echo
OBJDUMP=objdumpppc
AS=asppc
NM=nmppc
LD=ldppc

#
# TARGETS
# -------

# patch and configure are not repeated on calling again, therefore no separate build rule needed.
# Build the testcases in addition to the library and test driver.
# 'make check' builds the testcases as well as executes them.
# Therefore do the build as separate step. Not available as separate pseudotarget.
# List of testcases in log4cpp-$(LOG4CPP_VER)/tests/makefile.am not available before
# unpacking and patching. Therefore delegate into sub-make to be invoked after this.
# Need to treat 'make install' and 'make tests' special also. But 'make clean' already cleans the tests.
#skip default acsMakefile things
#all:	do_all
.PHONY : all
all: configure
	@echo ". . . building . . ."
	$(MAKE) -C log4cpp-$(LOG4CPP_VER) all
	$(MAKE) -f tests.Makefile all
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

# Remove results of configure
# Useful to rerun configure from scratch, or to check current state result before creating a new patch version
# Can be done only if Makefile got created during configuration
# Use Makefile existence as indicator.
# Cannot use make dependency, to avoid creating files which immediately get deleted again.
# Attention!
# If a configure run breaks before creating the Makefile, the file tree is inconsistent!
# In this case don't create a new patch untile configure was runing successfully!

.PHONY : clean_configure
clean_configure :
	@echo ". . . clean_configure . . ."
	@if [ -r log4cpp-$(LOG4CPP_VER)/Makefile ]; then \
		echo $(MAKE) -C log4cpp-$(LOG4CPP_VER) distclean; \
		$(MAKE) -C log4cpp-$(LOG4CPP_VER) distclean; \
	fi
	@echo " . . . clean_configure done"

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
# Allow 0 sized files remaining, because they are anyway not incorporated into the patch.
# Files deleted on lcu not checked.
# If the ws side of the package no longer exists this does no harm - on clean_dist lcu changes got checked in also.
# Even if on lcu side package was not extracted, create patch on ws side if extracted - maybe local changes there.
# Attention!
# Danger if the wrong autom4te.cache makes it from the ws to the lcu side which runs with a different configuration.
# autom4te.cache: Can be savely deleted. Created on certain configure steps. Turn off creation with one of
# autom4te.cfg ~/.autom4te.cfg ./.autom4te.cfg :
# begin-language: "Autoconf-without-aclocal-m4"
# args: --no-cache
# end-language: "Autoconf-without-aclocal-m4"

#skip default acsMakefile things
#clean_dist : clean_all clean_dist_all 
.PHONY : clean_dist 
clean_dist : clean_configure
	@echo ". . . clean_dist . . ."
	rm -f `find .. -name "*~"`
	@if [ -d log4cpp-$(LOG4CPP_VER) ]; then \
		echo rm -rf log4cpp-$(LOG4CPP_VER)/autom4te.cache; \
		rm -rf log4cpp-$(LOG4CPP_VER)/autom4te.cache; \
		if [ $$(find log4cpp-$(LOG4CPP_VER) -type f 2>&1 | wc -l) != 0 ]; then \
			echo ======== Error! ========; \
			find ../../lcu/src/log4cpp-$(LOG4CPP_VER) -type f -size 1; \
			echo files added and missing in ../../ws/src/log4cpp-$(LOG4CPP_VER); \
			echo if file is required, move to .../ws/... and place a corresponding symbolic link to it in .../lcu/...; \
			exit 1; \
		fi; \
	fi
	$(MAKE) -C ../../ws/src clean_dist
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

# Installation location can be overwritten at 'make install' as well as at 'configure'.
# See 'configure --help' and configure@"Installation directory options"
# LCU installation in general set by --prefix=$(INSTALL_DIR)/vw
# LCU binaries and libraries need to cover the CPU type also, therefore redirect separately
# --bindir==$(INSTALL_DIR)/vw/bin/$(CPU) --libdir=$(INSTALL_DIR)/vw/lib/$(CPU)
# Exception: Script log4cpp-config is meant for execution on host, but cannot be redirected
# separately. Therefore move to $(INSTALL_DIR)/bin afterwards.

#  go to $(INSTALL_DIR)/vw/bin/$(CPU) and $(INSTALL_DIR)/vw/lib/$(CPU)
# Therefore o 
 
#skip default acsMakefile things
# Install the testcases also, see rule 'all'.
# Fix the link before installing the executables
#install : install_all
.PHONY : install 
install : 
	@echo ". . . installing cppunit . . ."
	$(MAKE) -C log4cpp-$(LOG4CPP_VER) install
	mv $(INSTALL_DIR)/vw/bin/$(CPU)/log4cpp-config $(INSTALL_DIR)/bin/log4cpp-config
#	$(MAKE) -f tests.Makefile install
	@echo " . . . installation done"

# Folder name of patched version of extracted tgz. Create only if not existing, therefore no dependencies.
# Link over the unpacked and patched source from the ws branch.
# Before that bring the ws branch into the state ready for version management checkin.
# Reuse the Makefile rules of the ws branch for this.
# To be able to add lcu specific files to the lcu branch during configure and build,
# do not link the entire subtree in one piece, but duplicate the folder structure
# and 'ln -s' each file individually.
# See comments at clean_dist: autom4te.cache is unnecessary and can pull in ws
#  context when copied in. remove.
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
	@echo ". . . replicating to ../../lcu/src . . ."
	@for dir in $$(cd ../../ws/src; find log4cpp-1.0+ -type d); do \
		echo mkdir -p $$dir; \
		mkdir -p $$dir || exit 1; \
		echo ln -s -t $$dir $$(cd $$dir; find $$(sed -e "s#[^/]*#..#g" <<<$$dir )/../../ws/src/$$dir -maxdepth 1 -type f); \
		ln -s -t $$dir $$(cd $$dir || exit 1; find $$(sed -e "s#[^/]*#..#g" <<<$$dir || exit 1)/../../ws/src/$$dir -maxdepth 1 -type f || exit 1) || exit 1; \
	done
	rm -rf log4cpp-$(LOG4CPP_VER)/autom4te.cache
	@echo " . . . replicating to ../../lcu/src done"

# pseudo target name is an alias of the patched folder name. Create as dependency.
# unpack and cannot be a dependency of log4cpp-$(LOG4CPP_VER) but required, therefore
# prepend here as dependency.
.NOTPARALLEL :
.PHONY : patch 
patch : log4cpp-$(LOG4CPP_VER)

# File name of one configure result. Created by configure.
# Create only if not existing, therefore no dependencies.
# --enable-shared=no VxWorks kernel process supports only static libraries
# --enable-debug optional
# For destination locations see comment at "install" rule.

log4cpp-$(LOG4CPP_VER)/Makefile :
	@echo ". . . running configure . . ."
	echo "   log4cpp tar file is: log4cpp-$(LOG4CPP_TAR).tar.gz" >configure.log
	echo "   log4cpp version  is: $(LOG4CPP_VER)" >>configure.log
	cd log4cpp-$(LOG4CPP_VER); \
		./configure --enable-shared=no $(CONFIGURE_DEBUG) --host=powerpc-wrs-vxworks \
			--prefix=$(INSTALL_DIR)/vw --bindir=$(INSTALL_DIR)/vw/bin/$(CPU) --libdir=$(INSTALL_DIR)/vw/lib/$(CPU) \
			CC=$(CC) CXX=$(CXX) CPP=$(CPP) CXXCPP=$(CXXCPP) AR=$(AR) RANLIB=$(RANLIB) STRIP=$(STRIP) \
			OBJDUMP=$(OBJDUMP) AS=$(AS) NM=$(NM) LD=$(LD) DLLTOOL=$(DLLTOOL) \
			"CFLAGS=$(CFLAGS)" "CXXFLAGS=$(CXXFLAGS)" \
 			>>../configure.log 2>&1
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
# Run the the individual testcases also, see rule 'all'.
# 'make check' already available, also builds if not present.
# Todo: remote execution
#
.PHONY : test 	
test :
#	log4cpp-$(LOG4CPP_VER)/tests/testmain
#	log4cpp-$(LOG4CPP_VER)/tests/testmain unknownarg
#	log4cpp-$(LOG4CPP_VER)/tests/testbench 10 5
#	log4cpp-$(LOG4CPP_VER)/tests/testbench 10
#	log4cpp-$(LOG4CPP_VER)/tests/testbench
#	$(MAKE) -C log4cpp-$(LOG4CPP_VER) check
	@echo " . . . test done"

#___oOo___
