#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.96 2010/09/20 14:25:08 mzampare Exp $" 
#
# Makefile   for vlt customization of make 
#
# who       when        what
# --------  ----------  ----------------------------------------------
# psivera   2000-10-06  created 
# psivera   2001-06-27  added Makefile_LCU and Makefile_WS templates
#

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
# to set the shell to be used by panels
vltMakeSetPanelShell_OBJECTS   = 
vltMakeSetPanelShell_LDFLAGS   =
vltMakeSetPanelShell_LIBS      =


#
# Includes (.h) files (public only)
# ---------------------------------
INCLUDES = 

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
                     acsGetAllJars acsGetSpecificJars

PY_MODULES         =
PY_MODULES_L       =

PY_PACKAGES        =    acsConfigReportModule
PY_PACKAGES_L      =
pppppp_MODULES     =

#
# man pages to be done
# --------------------
MANSECTIONS = 1 5 
MAN1 = acsMan acsConfigReport.py
MAN5 = ../include/acsMakefile ../include/javaMakefile  ../include/rtaiMakefile ../include/idlMakefile ../include/xsdMakefile ../include/pythonMakefile Makefile.doc

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
all: do_all
	@echo " . . . 'all' done" 

clean : clean_all
	$(AT) $(RM) ../*~ ../*/*~
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean done"

man   : do_man
	@echo " . . . man page(s) done"

install : install_all
	cp ../include/Makefile_PACKAGE.template \
           ../include/Makefile_LCU.template \
           ../include/Makefile_WS.template    $(VLTTOP)/include

	@echo " . . . installation done"

#
#___oOo___
