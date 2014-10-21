#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.97 2012/09/20 12:09:55 eallaert Exp $" 
#
# Makefile   for acs customization of make 
#
# who       when        what
# --------  ----------  ----------------------------------------------
# psivera   2000-10-06  created 
# psivera   2001-06-27  added Makefile_LCU and Makefile_WS templates
# eallaert  2012-09-18  added acsSearchPath python script/function
# eallaert  2014-10-07  added acsMakeLogInstallation, acsMakeInstallFiles (copied from Kit/vlt)
#

#*******************************************************************************
# ALMA - Atacama Large Millimeter Array
# Copyright (c) ESO - European Southern Observatory, 2014
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
# This Makefile follows ACS Standards (see Makefile manpage documentation).
#
# REMARK: this should be the first module to be installed.
#         For such a reason, in few cases, it needs to work from /src.
#*******************************************************************************

#
# MODULE CODE DESCRIPTION:
# ------------------------
#
# C programs (public and local)
# -----------------------------
EXECUTABLES     =
EXECUTABLES_L   = 

#
# commands to manipulate Linux Loadable Kernel Modules



#
# Includes (.h) files (public only)
# ---------------------------------
INCLUDES = acsPort.h

#
# Scripts (public and local)
SCRIPTS  = 	\
                acsMakeTclScript	\
		acsMakeTclLib           \
		acsMan			\
		doxygenize		\
		acsChangeEnv            \
		acsSwitchEnv            \
		instAlmaTarball         \
                JacPrep			\
		acsCheckGroupPermissions\
                cvs2cl 			\
                acsUserConfig 		\
		acsMakeCheckUnresolvedSymbols \
		acsMakeJavaClasspath    \
		acsMakeCopySources      \
		acsMakeLogInstallation  \
		acsMakeInstallFiles     \
		searchFile


#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS  = acsReplace
acsReplace_OBJECTS  = acsReplace
acsReplace_TCLSH = tcl

#
# Python stuff (public and local)
# ----------------------------
PY_SCRIPTS         = acsConfigReport \
                     acsGetAllJars acsGetSpecificJars \
                     acsSearchPath

PY_MODULES         = acsSearchPath
PY_MODULES_L       =

PY_PACKAGES        =    acsConfigReportModule
PY_PACKAGES_L      =
pppppp_MODULES     =

#
# man pages to be done
# --------------------
MANSECTIONS = 1 5 
MAN1 = acsMan acsConfigReport.py
MAN5 = ../include/acsMakefile Makefile.doc

#
# local man pages
# ---------------
MANl = $(SCRIPTS)

#
# ASCII file to be converted into Framemaker-MIF
# --------------------
ASCII_TO_MIF = 


#
# other files to be installed
# ---------------------------
INSTALL_FILES = ../include/acsMakefile       \
		../include/acsMakefileDefinitions.mk \
		../include/acsMakefileCore.mk \
                ../include/Makefile_LCU.template \
                ../include/Makefile_WS.template \
                ../include/Makefile_PACKAGE.template \
                $(wildcard ../config/acsPackageInfo*.rpmref) \
		../config/XSDIncludeDependencies.xml
#
# list of all possible C-sources (used to create automatic dependencies)
# ------------------------------
CSOURCENAMES =\
	$(foreach exe, $(EXECUTABLES) $(EXECUTABLES_L), $($(exe)_OBJECTS)) \
	$(foreach lib, $(LIBRARIES) $(LIBRARIES_L), $($(lib)_OBJECTS))

#
#>>>>> END OF standard rules

#
# CHOSE PLATFORM
# --------------
# Default is UNIX, for VxVorks application next line MUST BE UNCOMMENTED
#MAKE_VXWORKS = on   

#
# INCLUDE STANDARDS
# -----------------
# 
# REMARK: the local file (in /src) is used, so it can work also for
# installation on an empty system.
#
PATH:=$(PATH):.
MAKEDIR = ../include
include $(MAKEDIR)/acsMakefile

#
# TARGETS
# -------
all: do_all do_acsPort
	@echo " . . . 'all' done" 

clean : clean_all
	$(AT) $(RM) ../*~ ../*/*~
	-$(AT) cp ../include/acsPort.h.DUMMY ../include/acsPort.h
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean done"

man   : do_man
	@echo " . . . man page(s) done"

# vltPort.h is a symlink to acsPort.h, which is there merely to deal with "old" source
# code that refers to vltPort.h instead of acsPort.h
install : install_all
	$(AT)cp ../include/Makefile_PACKAGE.template \
           ../include/Makefile_LCU.template \
           ../include/Makefile_WS.template    $(PRJTOP)/include
	$(AT)(cd $(PRJTOP)/include; rm -f vltPort.h; ln -s acsPort.h vltPort.h)

	@echo " . . . installation done"

#
# according to current platform, set the proper acsPort.h
# 
do_acsPort:
	-$(AT) chmod 666   ../include/acsPort.h; cp ../include/acsPort.h.UNSUPPORTED ../include/acsPort.h; chmod 666   ../include/acsPort.h
	-$(AT) if [ `uname` = Linux ]; then cp ../include/acsPort.h.Linux ../include/acsPort.h; chmod 666 ../include/acsPort.h; fi
	-$(AT) if [ `uname` = $(CYGWIN_VER) ]; then cp ../include/acsPort.h.Cygwin ../include/acsPort.h; chmod 666 ../include/acsPort.h; fi

#
#___oOo___
