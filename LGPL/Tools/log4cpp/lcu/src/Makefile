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
# "@(#) $Id: Makefile,v 1.6 2011/02/21 11:26:21 htischer Exp $"
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
# htischer  11/02/11  building for, loading and running rpeatedly and reproducable on VxWorks.
#					  Remote execution of testsuite, processing of test log for easier diff.
#					  Todo: Verify detailed test results.
#                     Note: Treat library as application to load once in common address space.
#					  Linking all testcases into a single application without library
#                     testxxx.cpp:main(argc,argv) turned into testxxx.cpp:testxxx(char *args)
#					  Rename 'make clean' to 'make clean_build' and 'make clean_configure' to 'make clean' 

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
# make options:
# OPTIMIZE=on		for configure, deactivate debugging
# make targets:
#
# all			Build. Do unpack, patch and configure if and only if necessary.
#
# clean_dist	Bring subtree in a state suitable for version control checkin.
#				This includes also creating a new patch version in case of
#				modifications in the extracted sourcecode.
#
# clean			remove configure results, if they got created.
#				Avoids doing a configure just to be able to do a distclean.
#				Useful to start configure from scratch or checking current state
#				before creating a new patch. 
#
# clean_build	Prepares for a clean rebuild, but leaves configure results.
#
# patch			patch the original log4cpp package with sourcecode updates.
#				unpack when necessary.
#
# configure		autoconfigure the build to the current runtime/build environment.
#				Unpack and patch when necessary.
#
# test			Run the provided test suite on the remote host
#
# reboot	    Reboot the remote host, optionally before a new test
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

# undef as much as possible include paths which are unnecessary for a plain Posix/VxWorks application.
# Attention! if ACE_ROOT_VW is cleared, special C++ LD must be restored after inclusion of acsMakefile!
# VLTROOT clearing has no relevant effect (except shortening the include path a litle)
# See more at CONFIGURE_CFLAGS

# List of files to be linked
#LINK_FILES =
#

#
# user definable C-compilation flags
# See more at CONFIGURE_CFLAGS
# on use of VxWorks headers avoid harmless but confusing compiler warning.
# No effect on plain Posix app.
# USER_CFLAGS =-D_VSB_CONFIG_FILE=\"$(WIND_BASE)/target/lib/h/config/vsbConfig.h\"
#
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

EXECUTABLES     = log4cpptests log4cppcommon

#EXECUTABLES_L   = 

#
# <brief description of xxxxx program>
#xxxxx_OBJECTS   =	
#xxxxx_LDFLAGS   =
#xxxxx_LIBS      =

#cpp.cpp: empty dummy object to trigger C++ linking,
#because to vltMakefile this is not obvious for the externally compiled sources
#For acsMakefile with ACE_ROOT_VW unnecessary, because always linking for C++ by the ace_ld wrapper.
#log4cpptests_OBJECTS   = cpp
#log4cppcommon_OBJECTS = cpp
#example is an external object
#Before do_all need to create files expected by acsMakefile, but cannot be created
# by acsMakefile because of missing source.
# The preceding 'lib' of the 'libxxx.a' must be omitted here!
# To be on the save side don't name an application like the library with ommited 'lib'!
#tests_OBJECTS   = cpp example
#tests_LIBS = local
#common_OBJECTS = cpp
#common_LIBS = log4cpp
#common_LDFLAGS = --whole-archive
# all: example do_all
# ...
# .PHONY: example
#	touch cpp.cpp
#	ln -ft ../object -s ../src/log4cpp-$(LOG4CPP_VER)/tests/example.o
#	touch ../object/example.d
#	ln -ft ../lib -s ../src/log4cpp-$(LOG4CPP_VER)/src/.libs/liblocal.a
# Create external libraries to be referenced.
# Application specific library: here
# Common library: Create as an extra application to download separately.
# By --whole-archive ink in everything, because not known by later apps.
# Application link without common library.
# That way library can be loaded exactly once into the single kernel process for
# more applications.
#
# Here for simplicifaction:
# Put all application modules into a single executable.

log4cpptests_LIBS = tests
log4cpptests_LDFLAGS = --whole-archive

log4cppcommon_LIBS = log4cpp
log4cppcommon_LDFLAGS = --whole-archive

#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

#
# Includes (.h) files (public only)
# ---------------------------------
#INCLUDES        =

#
# Libraries (public and local)
# For now we link the entire log4cpp library to a downloadable application
# to ensure one time download.
# If it is sure, that log4cpp is used by exactly one application, alternatively
# it could be statically linked to that application. In this case install the .a library here.
# Note: So far installed anyway by 'configure' created install procedure. Consider to rm again.
# ----------------------------
#LIBRARIES       =
#LIBRARIES_L     =

#
# <brief description of lllll library>
#lllll_OBJECTS   =

#
# Scripts (public and local)
# ----------------------------
#SCRIPTS         =
#SCRIPTS_L       =

#
# TCL scripts (public and local)
# ------------------------------
#TCL_SCRIPTS     =
#TCL_SCRIPTS_L   =

#
# <brief description of tttttt tcl-script>
#tttttt_OBJECTS  =
#tttttt_TCLSH    = 
#tttttt_LIBS     = 

#
# TCL libraries (public and local)
# ------------------------------
#TCL_LIBRARIES   =
#TCL_LIBRARIES_L =

#
# <brief description of tttlll library>
#tttlll_OBJECTS  = 

#
# UIF panels (public and local)
# ----------------------------
#PANELS   =
#PANELS_L = 

#
# IDL Files and flags
#
#IDL_FILES =
#IDL_TAO_FLAGS =
#USER_IDL =

#
# man pages to be done
# --------------------
#MANSECTIONS =
#MAN1 =
#MAN3 =
#MAN5 =
#MAN7 =
#MAN8 =

#
# local man pages
# ---------------
#MANl =

#
# ASCII file to be converted into Framemaker-MIF
# --------------------
#ASCII_TO_MIF = 

#
# INS_ROOT files to be installed
#-------------------------------
#INS_ROOT_FILES =
#INS_ROOT_DIR   =

#
# other files to be installed
#----------------------------
#INSTALL_FILES =

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
MAKE_VXWORKS = on   

#
# INCLUDE STANDARDS
# -----------------

MAKEDIRTMP := $(shell searchFile include/acsMakefile)
ifneq ($(MAKEDIRTMP),\#error\#)
   MAKEDIR := $(MAKEDIRTMP)/include
   include $(MAKEDIR)/acsMakefile
endif

# acsMakefile with ACE_ROOT_VW adds -fcheck-new to the ctdt.c compilation of the munch step,
# like on other c/cpp files, causing:
# cc1: warning: command line option "-fcheck-new" is valid for C++/ObjC++ but not for C
# removing the option from $(LD), which contains the cpp arguments for munch also.

LD := $(filter-out -fcheck-new,$(LD))

# acsMakefile for VxWorks behaviour is
# - if DEBUG is defined, always compile for debug, independent of additional optimization
# - if OPTIMIZE is not defined, always compile for debug
# configure behaviour os
# - if compile for debug the no optimization, otherwise -O2
# give the debug request precedence
# explicitely define common for with and without acsMakefile.
# On acsMakefile use we could deduct final debug mode by ifeq ($(filter -g,$(CFLAGS)),)...
# Use CONFIGURE_DEBUG also as debug indicator further below
CONFIGURE_DEBUG=
ifeq ($(OPTIMIZE),)
CONFIGURE_DEBUG=--enable-debug
endif
ifneq ($(DEBUG),)
CONFIGURE_DEBUG=--enable-debug
endif

CONFIGURE_acsMakefile = on

ifneq ($(CONFIGURE_acsMakefile),)

# acsMakefile VxWorks Kernel Module options.
#
# Final composition of options and commands for configure
# Using acsMakefile settings
# From acsMakefile '../object/%.o : ../object/%.cpp' template
# Before configure remove options added automatically by configure, see below

CONFIGURE_CFLAGS=$(CFLAGS) $(I_PATH) $($*_CFLAGS)
CONFIGURE_CXXFLAGS=$(CFLAGS) $(CPPFLAGS) $(C_PLUS_PLUS_FLAGS) $(I_PATH) $($*_CFLAGS)

else

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
#   LD = perl $(ACE_ROOT)/bin/ace_ld -C ccppc $(CFLAGS) -- -m munch -n nm$(VX_CPU_FAMILY) ld$(VX_CPU_FAMILY) -X -r -T $(WIND_BASE)/target/h/tool/gnu/ldscripts/link.OUT
# Attention! Only if ACE_ROOT_VW is defined, the C++ compatible link is done! (internally translated to ACE_ROOT)
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
ifeq ($(CONFIGURE_DEBUG),)
WR_CFLAGS_OPTIMIZE=-O2 -fstrength-reduce -fno-builtin
else
WR_CFLAGS_OPTIMIZE=-g
endif

# Only for normal source files, but not for ctdt.c
WR_CFLAGS_SOURCE=-ansi -MD -MP

# Common for all compilations
WR_CFLAGS_COMMON=-mcpu=604 -mstrict-align -fno-implicit-fp -Wall \
-I$(WIND_BASE)/target/h -I$(WIND_BASE)/target/h/wrn/coreip \
-DCPU=PPC604 -DTOOL_FAMILY=gnu -DTOOL=gnu -D_WRS_KERNEL \
-D_VSB_CONFIG_FILE=\"$(WIND_BASE)/target/lib/h/config/vsbConfig.h\"

# compilation of ctdt file, C++ file level constructor/destructor list after the "munch" step
# -ansi -MD -MP not needed for ctdt. Instead add -fdollars-in-identifiers
WR_CFLAGS_CTDT=$(WR_CFLAGS_OPTIMIZE) $(WR_CFLAGS_COMMON) -fdollars-in-identifiers

# only for intermediate link for reuse result in later link step
WR_CFLAGS_LINKPARTIAL=

# only for final link of downloable kernel module, picking up intermediate and munch results
WR_CFLAGS_LINKFINAL=-T $(WIND_BASE)/target/h/tool/gnu/ldscripts/link.OUT

# common for both link steps 
WR_CFLAGS_LINK=-r -nostdlib -Wl,-X

# -----

# Final composition of options and commands for configure

# C/C++ compiler options
# configure adds -ansi -MD -MP -Wall automatically.
# See configure.in: depending on --enable-debug, GCC_CFLAGS -g/-O added automatically
# In addition it adds -Wno-unused -pedantic and -D_DEBUG/-DNDEBUG
#
# Note: C compiler warning
# 'warning: #warning "VxWorks Source Build (VSB) project not specified; \
# using default VxWorks UP configuration under $WIND_BASE/target/lib/h/config"'
# This occures if an application includes a VxWorks specific non Posix header file, e.g. <taskLib.h>
# and is completely harmeless.
# This should not happen with this application which was imported from Linux.
# If warning occures after a sourcecode change, suppress it by following compiler argument, e.g. by USER_CFLAGS:
# -D_VSB_CONFIG_FILE=\"$(WIND_BASE)/target/lib/h/config/vsbConfig.h\"
#
# Note:
# Include path imported by acsMakefile: TAO, ACSSW, CCSLite, tcltk, relative paths, introot -
# are not necessary here with application imported from plain Linux, but they do no harm.
# Consider to remove them below.
# Easy to remove CCSLite by deleting VLTROOT at beginning of makefile
# TCL_ROOT cannot be undefined.
# TAO cannot simply removed, because the C++ compatible link is done only if ACE_ROOT got active by ACE_ROOT_VW
#  A method would be:
#  - at beginning of Makefile:
#    ACE_ROOT_FOR_LD=$(ACE_ROOT_VW)
#    ACE_ROOT_VW=
#  - after inclusion of acsMakefile
#    LD = perl $(ACE_ROOT_FOR_LD)/bin/ace_ld ... continue like acsMakefile setting 
# Or alternatively do the entire compilation using the WR_* flags above.
#
# Before configure, remove the options which got added automatically by configure, see below
#
# ACE_ROOT_VW propagates to ACE_ROOT also by acsMakefile. In this case additional compiler settings are active:
#  CFLAGS :=   -ansi -pipe -D_REENTRANT -fno-builtin -fno-defer-pop -fvolatile -nostdlib -DACE_VXWORKS=0x670 -Wno-uninitialized -DACE_LACKS_RTTI  -fcheck-new $(CFLAGS)
#  LD = perl $(ACE_ROOT)/bin/ace_ld -C ccppc $(CFLAGS) -- -m munch -n nm$(VX_CPU_FAMILY) ld$(VX_CPU_FAMILY) -X -r -T $(WIND_BASE)/target/h/tool/gnu/ldscripts/link.OUT

CONFIGURE_CFLAGS=$(WR_CFLAGS_OPTIMIZE) $(WR_CFLAGS_COMMON) $(WR_CFLAGS_SOURCE)
CONFIGURE_CXXFLAGS=$(CONFIGURE_CFLAGS)

endif

# Remove options added by configure, in particular -c to allow optional linking and -g/-Ox by --enable-debug
# See below in WR options case

CONFIGURE_AUTOCFLAGS=-c -O -O0 -O1 -O2 -O3 -g -ansi -MD -MP -Wall

CONFIGURE_CFLAGS:=$(filter-out $(CONFIGURE_AUTOCFLAGS),$(CONFIGURE_CFLAGS))
CONFIGURE_CXXFLAGS:=$(filter-out $(CONFIGURE_AUTOCFLAGS),$(CONFIGURE_CXXFLAGS))

#ccppc routes the request through to c++ppc when necessary by options or file type.
#By convention with C++, VxWorks RTP and VLT/APEX for VxWorks Kernel are using c++ppc,
#and Wind River builds for VxWorks Kernel are using ccppc
#For now follow the Wind River VxWorks Kernel convention 
##CXX=c++ppc
#CXX=ccppc
#CC=ccppc
# Same Preprocessor for C and C++, behaviour adjustment by options and file type.
#CPP=cppppc
#CXXCPP=cppppc
#used?
#AR=arppc
#RANLIB=ranlibppc
#STRIP=stripppc
#Not yet needed, but set to be correct in the auto-generated ./libtool
#DLLTOOL not available for kernel modules in Vxworks
#DLLTOOL=exit 1
#OBJDUMP=objdumpppc
#AS=asppc
#NM=nmppc
#LD=ldppc
#For sure the LD setting confuses acsMakefile LD=perl ... setting.
#To avoid conflicts use different variables to hold the VxWorks tool names for configure.
#For convenience, all in one.

CONFIGURE_TOOLS= CC=ccppc CXX=ccppc CPP=cppppc CXXCPP=cppppc AR=arppc RANLIB=ranlibppc STRIP=stripppc \
			OBJDUMP=objdumpppc AS=asppc NM=nmppc LD=ldppc "DLLTOOL=exit 1" \

#
# TARGETS
# -------

# Before do_all need to create expected by acsMakefile by configure, configure-build and postprocess.
# global NOTPARALLEL to impose 'list order of prerequisites is execution order' without additional pseudo targets.
.NOTPARALLEL :
.PHONY : all
all: configure compile do_all
	@echo " . . . 'all' done" 

# - patch and configure are not repeated on calling again, therefore no separate build rule needed.
#   '-C log4cpp-$(LOG4CPP_VER) all' builds only library and testmain and testbench object modules.
#   executables are not usable for VxWorks.
# - link the created external library to a place more appropriate for acsMakefile
# - Before very first run of do_all, folder for external might not exist. Create to be sure.
# -  Build the testcases in addition to the library and test driver.
#   'make check' builds the testcases as well as executes them.
#   Therefore do the build as separate step. Not available as separate pseudotarget.
#   List of testcases in log4cpp-$(LOG4CPP_VER)/tests/makefile.am not available before
#   unpacking and patching. Therefore delegate into sub-make to be invoked after this.
#   The submake creates also a library ../lib/libtests.a with test application main code.
# - dummy cpp.cpp no longer needed, because acsMakefile with ACE_ROOT_VW always uses C++ link, see above.
#	touch cpp.cpp	
# Note: Need to treat 'make install' and 'make tests' special also. But 'make clean_build' already cleans the tests.
#
# Also create expect script to reboot the VxWorks remote host.

compile:
	@echo ". . . compiling . . ."
	$(MAKE) -C log4cpp-$(LOG4CPP_VER) all
	mkdir -p ../lib
	ln -ft ../lib -s ../src/log4cpp-$(LOG4CPP_VER)/src/.libs/liblog4cpp.a
	$(MAKE) -f tests.Makefile all
	mkdir -p ../bin
	@echo "echo '...remote reboot script...' >../bin/reboot.expect"
	@{ \
	echo "#!/usr/bin/expect -f"; \
	echo "set xhost [lindex \$$argv 0]"; \
	echo "set timeout 60\r"; \
	echo "spawn rlogin \$$xhost"; \
	echo "expect -ex \"\\n-> \""; \
	echo "send \"reboot\r\r\r\r\r\""; \
	echo "expect -ex \"Connection reset by peer\""; \
	echo "spawn rlogin \$$xhost"; \
	echo "send \"\r\r\r\r\""; \
	echo "expect -ex \"\\n-> \""; \
	echo "close"; \
	echo "wait"; \
	} >../bin/reboot.expect
	@[ -r ../bin/reboot.expect ] || exit 1
	chmod a+x ../bin/reboot.expect
	@echo " . . . 'compile' done" 

# Additional files created by compile including libtests.a by 'tests.Makefile all'
# dummy cpp.cpp no longer needed, see above.
.PHONY : clean_compile 
clean_compile :
	rm -rf ../lib/libtests.a ../lib/liblog4cpp.a ../bin/rtests.expect ../bin/reboot.expect

# do clean of default acsMakefile things before cleaning special files, see target 'compile'.
.PHONY : clean_build 
clean_build : clean_compile clean_all 
	@if [ -r log4cpp-$(LOG4CPP_VER)/Makefile ]; then \
		echo $(MAKE) -C log4cpp-$(LOG4CPP_VER) clean; \
		$(MAKE) -C log4cpp-$(LOG4CPP_VER) clean; \
	fi
	@echo " . . . clean_build done"

# Remove results of configure
# Useful to rerun configure from scratch, or to check current state result before creating a new patch version
# Can be done only if Makefile got created during configuration
# Use Makefile existence as indicator.
# Cannot use make dependency, to avoid creating files which immediately get deleted again.
# Attention!
# If a configure run breaks before creating the Makefile, the file tree is inconsistent!
# In this case don't create a new patch untile configure was runing successfully!
# '-C log4cpp-$(LOG4CPP_VER) distclean' covers also 'log4cpp-$(LOG4CPP_VER) clean', but not
# the extra files by target 'compile'.
# After clean no reuse of old acsMakefile results - clean 

.PHONY : clean
clean : clean_build
	@echo ". . . clean . . ."
	@if [ -r log4cpp-$(LOG4CPP_VER)/Makefile ]; then \
		echo $(MAKE) -C log4cpp-$(LOG4CPP_VER) distclean; \
		$(MAKE) -C log4cpp-$(LOG4CPP_VER) distclean; \
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
# clean_dist_all after clean_all for acsMakefile created empty folders. log4cpp package unrelated, do afterwards.
# clean_all contained in clean.
# ../test created by acsMakefile, but not cleaned up. testcases currently not standalone, but part of ../src

#modify default acsMakefile things
#clean_dist : clean_all clean_dist_all 
.PHONY : clean_dist 
clean_dist : clean clean_dist_all
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
	rm -rf log4cpp-$(LOG4CPP_VER) configure.log log4cpp_teststimes.log log4cpp_tests.log log4cpp_testsyslog.log log4cpp_testmain.log A1.log sub1.log ../test
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

# Install the testcases also in addition to the log4cpp results, see 'make all'.
# For the testcases need also runtime files to be loaded.
# Fix the host script location created by log4cpp
# Attention! The host script log4cpp-config is still depending on configure parameter and shared with ws build!
# store under a different name!
.PHONY : install 
install : install_all
	@echo ". . . installing log4cpp . . ."
	$(MAKE) -C log4cpp-$(LOG4CPP_VER) install
	mv $(INSTALL_DIR)/vw/bin/$(CPU)/log4cpp-config $(INSTALL_DIR)/bin/log4cpp-config_vw
	$(MAKE) -f tests.Makefile install
	cp -p ../bin/rtests.expect $(INSTALL_DIR)/bin/rtests.expect
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
#	for dir in `find log4cpp-$(LOG4CPP_VER) -type d`; do \
#		echo ln -s -t $$dir `find $$STARTPWD/../../ws/src/$$dir -maxdepth 1 -type f`; \
#		ln -s -t $$dir `find $$STARTPWD/../../ws/src/$$dir -maxdepth 1 -type f`; \
#	done
# relative symbolic links, nested commands on echo not expanded, mkdir not in inner loop
#   mkdir -p $$(cd ../../ws/src; find log4cpp-$(LOG4CPP_VER) -type d)
#	@for dir in `find log4cpp-$(LOG4CPP_VER) -type d`; do \
#		echo ln -s -t $$dir \$$\(cd $$dir\; find \$$\(sed -e "s#[^/]*#..#g" \<\<\<$$dir \)/../../ws/src/$$dir -maxdepth 1 -type f\); \
#		ln -s -t $$dir $$(cd $$dir; find $$(sed -e "s#[^/]*#..#g" <<<$$dir )/../../ws/src/$$dir -maxdepth 1 -type f); \
#	done
#
# Todo:
# unnecessary execution of autoconf and automake or warning messages
# 'missing --run automake-1.10', 'missing --run autoconf' etc
# This means that a dependent file has a newer timestamp after unpacking and patching from the archive.
# Non available autoconfigure tool is simulated by simply touching the dependent files.
# See ./config/missing actions, and $(AUTOCONF) use in top Makfile to see when it is executed.
# In particular $(top_srcdir)/m4/ACX_PTHREAD.m4 and configure got patched consistently.
# configure must be newer.
#
log4cpp-$(LOG4CPP_VER) :
	$(MAKE) -C ../../ws/src clean
	$(MAKE) -C ../../ws/src patch
	@echo ". . . replicating to ../../lcu/src . . ."
	@for dir in $$(cd ../../ws/src; find log4cpp-$(LOG4CPP_VER) -type d); do \
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
			$(CONFIGURE_TOOLS) \
			"CFLAGS=$(CONFIGURE_CFLAGS)" "CXXFLAGS=$(CONFIGURE_CXXFLAGS)" \
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

# Executing all individual tests on VxWorks remote host
# Need hostname as make Variable REMOTE_HOST
#
# Not useful here, because rsh/ftp do not support append mode of files:
#   Assuming installation folder mounted to same place on remote host
#   Assuming home folder of remote user is writable and mounted since boot via rsh or ftp.
#   Assumes that tests and test execution script are already built and installed.
#   For "~" remote home folder as writeable folder, escape to not expanding by local shell!
#     see tests.Makefile
# Instead of rsh/ftp, create a nfs mount on the remote host for the remote accessible files.
#   Below this provide a folder which is writeable for the remote user for temporary files.
#   The remote host needs to know the name of the local workstation for nfsMount.
#   Pass this instead of the now unused "~" for the tmp folder.
#
# After the test save the results in the current folder and remove the temp folder for security reasons.
#
# redirect test output into file
# For easier diff of results remove time stamps
# log4cpp_tests.log
.PHONY : tests 	
tests :
	@if [ x$(REMOTE_HOST) == x ]; then echo Error: 'REMOTE_HOST=xxx' missing!; exit 1; fi
	rm -rf $(INSTALL_DIR)/vw/bin/$(CPU)/log4cpp_temp
	mkdir -p $(INSTALL_DIR)/vw/bin/$(CPU)/log4cpp_temp
	chmod a+w $(INSTALL_DIR)/vw/bin/$(CPU)/log4cpp_temp
	../bin/rtests.expect $(REMOTE_HOST) $(INSTALL_DIR)/vw/bin/$(CPU) `hostname` `date +%s`| tee log4cpp_teststimes.log
	sed \
	-e "s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]/XXXX-XX-XX/" \
	-e "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9],[0-9][0-9][0-9]/XX:XX:XX,XXX/" \
	-e "s/[0-9][0-9] [A-Z][A-Za-z][A-Za-z] [0-9][0-9][0-9][0-9]/XX XXX XXXX/" \
	-e "s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]/XX:XX:XX.XXX/" \
	-e "s/[0-9][0-9]*\.*[0-9]* us/XX.XX us/" \
	-e "s/^[0-9][0-9][0-9][0-9]*/XXX/" \
	log4cpp_teststimes.log > log4cpp_tests.log
	mv -ft . $(INSTALL_DIR)/vw/bin/$(CPU)/log4cpp_temp/log4cpp_testsyslog.log $(INSTALL_DIR)/vw/bin/$(CPU)/log4cpp_temp/log4cpp_testmain.log $(INSTALL_DIR)/vw/bin/$(CPU)/log4cpp_temp/A1.log  $(INSTALL_DIR)/vw/bin/$(CPU)/log4cpp_temp/sub1.log
	rm -rf $(INSTALL_DIR)/vw/bin/$(CPU)/log4cpp_temp
	@echo " . . . remote test done"

# Reboot the remote host
.PHONY : reboot
reboot :
	@if [ x$(REMOTE_HOST) == x ]; then echo Error: 'REMOTE_HOST=xxx' missing!; exit 1; fi
	../bin/reboot.expect $(REMOTE_HOST)
	@echo " . . . remote reboot done"

#___oOo___
