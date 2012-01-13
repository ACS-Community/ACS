#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.4 2012/01/13 18:54:47 tstaig Exp $" 
#
# Makefile   for vlt customization of make 
#
# who       when      what
# --------  --------  ----------------------------------------------
# almamgr 2002-03-01 Added support for Solaris 5.8 for vltPort.h.
# gfilippi  29/06/93  created 
# gfilippi  29/06/93  use local vltMakefile (needed at the 1st installation)
# gfilippi  26/11/93  extended to VxWorks
# gfilippi  30/11/93  makeCopySources added
# gfilippi  18/02/94  vltDirectoryStructure.doc moved to templates
# gfilippi  01/05/94  vltPort.h added to INCLUDES
# gfilippi  19/05/94  makeTcl and makeTclLib added
# gfilippi  05/08/94  makeLogInstall added
# gfilippi  07/09/94  changed file names according to standards
# gfilippi  23/10/94  changed according to Makefile.template
# gfilippi  24/10/94  vltMakeScriptDependencies added
# gfilippi  02/11/94  RtapUnlockExe added
# gfilippi  09/02/95  files from vltMan added
# gfilippi  09/02/95  vltMakeInstallErrorFiles added
# gfilippi  17/02/95  vltMakePanelDependencies added
# gfilippi  17/02/95  setmod.csh renamed as vltSetmod.csh
# gfilippi  02/03/95  vltMakeCleanDB added
# gfilippi  04/03/95  vltMakeInstallTableFiles added
# gfilippi  06/03/95  chmod added in install_vltMake_filesinstall
# gfilippi  03/07/95  vltMakeSetPanelShell, vltMakeInstallLogFile added
# gfilippi  07/07/95  vltMakeLOGSFiles added
# gfilippi  11/07/95  vltMakeIndex added
# gfilippi  17/11/95  streambuf.h added
# gfilippi  20/11/95  vltMakeIndex, vltMakeLOGSFiles removed
# gfilippi  20/11/95  vltMakeIndexFiles added
# gfilippi  16/12/95  vltMakeInstallFiles added
# gfilippi  16/12/95  vltMakefile* moved to ../include
# gfilippi  10/04/96  streambuf.h removed (now libgcc++ provides a correct one)
# gfilippi  11/05/96  vltMakeSetPanelShell manually moved to ../bin (needed the 1st time)
# gfilippi  22/05/96  create vltPort.h at run time
# gfilippi  27/02/97  HP10.20 added
# gfilippi  14/04/97  SunOS2.5 added, HP-9.x removed
# gfilippi  05/05/97  RtapUnlockExe removed
# gfilippi  08/08/97  SunOS2.5.1 added
# gfilippi  23/11/97  HP10.10 dismissed
# tebert    16/07/98  Added handling of struct timeval for CCS-LITE II/Solaris
# wpirani   01/04/99  Added test case(s) for Solaris 2.6
# rschmutz 1999-04-03 vltMakeScript added.
# gfilippi  23/06/99  vltMakeInstallAlarmFiles added (SPR 990257)
# rschmutz  30/09/99  vltMakeInstallInsRootDir, vltMakeInstallInsRootFiles
#			added (SPR 990346).
# psivera   10/04/00  ported to HPUX-11
# psivera   25/07/00  added vltMakeIdlDependencies 
# psivera   06/06/01  added a Makefile_WS.template and Makefile_LCU.template, removed 
#                     the Makefile.template
# psivera   08/06/02  removed vltMakefile.idl from INSTALL_FILES
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile manpage documentation).
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
EXECUTABLES     = vltMakeSetPanelShell
EXECUTABLES_L   = 

#
# to set the shell to be used by panels
vltMakeSetPanelShell_OBJECTS   = vltMakeSetPanelShell
vltMakeSetPanelShell_LDFLAGS   =
vltMakeSetPanelShell_LIBS      =


#
# Includes (.h) files (public only)
# ---------------------------------
INCLUDES = vltPort.h 


#
# Scripts (public and local)
SCRIPTS  = 	vltMakeExecutableDependencies	\
		vltMakeLibraryDependencies 	\
		vltMakeScriptDependencies       \
		vltMakeScript			\
		vltMakePythonModDependencies    \
		vltMakePythonModule		\
		vltMakePythonPackDependencies   \
		vltMakePythonPackage		\
		vltMakeJavaDependencies		\
		vltMakeJavaClasspath		\
		vltMakeTclScriptDependencies 	\
		vltMakeTclScript                \
		vltMakeTclLibDependencies 	\
		vltMakeTclLib			\
		vltMakePanelDependencies        \
		vltMakeCopySources		\
                vltMakeLogInstallation          \
                vltMakeInstallErrorFiles        \
                vltMakeInstallAlarmFiles        \
                vltMakeInstallTableFiles        \
                vltMakeInstallLogFiles          \
                vltMakeInstallFiles             \
		vltMakeInstallInsRootDir	\
		vltMakeInstallInsRootFiles	\
                vltMakeCleanDB                  \
                vltMakeIndexFilesDependencies   \
                vltMan 				\
                vltMakeIdlDependencies 		\
                vltSetmod.csh                   \
                searchFile

#
# man pages to be done
# --------------------
MANSECTIONS = 1 5 7
MAN1 = vltMan
MAN5 = ../include/vltMakefile Makefile.doc ../include/javaMakefile
MAN7 = vltSetmod.csh

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
INSTALL_FILES = ../include/vltMakefile       \
                ../include/javaMakefile   \
                ../include/pythonMakefile   \
                ../include/idlMakefile   \
                ../include/tclMakefile   \
                ../include/vltMakefile.all   \
                ../include/Makefile_WS.template \
                ../include/Makefile_LCU.template

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
MAKEDIR = ../include
include $(MAKEDIR)/vltMakefile

#
# TARGETS
# -------
all: directory_structure do_all move_vltMakeSetPanelShell do_vltPort
	@echo " . . . 'all' done" 

#
# when installing for the first time, the file is created locally, so an explicit
# mv is given to allow make install to find it in ../bin 
move_vltMakeSetPanelShell:
	-$(AT) if [ -f vltMakeSetPanelShell ] ; then mv vltMakeSetPanelShell ../bin ; fi
#
# according to current platform, set the proper vltPort.h
# 
do_vltPort:
	-$(AT) chmod 666   ../include/vltPort.h; cp ../include/vltPort.h.UNSUPPORTED ../include/vltPort.h; chmod 666   ../include/vltPort.h
	-$(AT) if [ `uname -r` = B.11.00 ]; then cp ../include/vltPort.h.HPUX-11     ../include/vltPort.h; chmod 666 ../include/vltPort.h; fi
	-$(AT) if [ `uname -r` = B.10.20 ]; then cp ../include/vltPort.h.HPUX-10     ../include/vltPort.h; chmod 666 ../include/vltPort.h; fi
	-$(AT) if [ `uname -r` = 5.4     ]; then cp ../include/vltPort.h.Solaris.2   ../include/vltPort.h; chmod 666 ../include/vltPort.h; fi
ifdef RTAPROOT
	-$(AT) if [ `uname -r` = 5.5     ]; then cp ../include/vltPort.h.Solaris.2   ../include/vltPort.h; chmod 666 ../include/vltPort.h; fi
	-$(AT) if [ `uname -r` = 5.5.1   ]; then cp ../include/vltPort.h.Solaris.2   ../include/vltPort.h; chmod 666 ../include/vltPort.h; fi
	-$(AT) if [ `uname -r` = 5.6     ]; then cp ../include/vltPort.h.Solaris.2   ../include/vltPort.h; chmod 666 ../include/vltPort.h; fi
	-$(AT) if [ `uname -r` = 5.8     ]; then cp ../include/vltPort.h.Solaris.2   ../include/vltPort.h; chmod 666 ../include/vltPort.h; fi
else
	-$(AT) if [ `uname -r` = 5.5     ]; then cp ../include/vltPort.h.Solaris.2.lite   ../include/vltPort.h; chmod 666 ../include/vltPort.h; fi
	-$(AT) if [ `uname -r` = 5.5.1   ]; then cp ../include/vltPort.h.Solaris.2.lite   ../include/vltPort.h; chmod 666 ../include/vltPort.h; fi
	-$(AT) if [ `uname -r` = 5.6     ]; then cp ../include/vltPort.h.Solaris.2.lite   ../include/vltPort.h; chmod 666 ../include/vltPort.h; fi
	-$(AT) if [ `uname -r` = 5.8     ]; then cp ../include/vltPort.h.Solaris.2.lite   ../include/vltPort.h; chmod 666 ../include/vltPort.h; fi
endif
	-$(AT) if [ `uname` = Linux ]; then cp ../include/vltPort.h.Linux ../include/vltPort.h; chmod 666 ../include/vltPort.h; fi
	-$(AT) if [ `uname` = $(CYGWIN_VER) ]; then cp ../include/vltPort.h.Cygwin ../include/vltPort.h; chmod 666 ../include/vltPort.h; fi

clean : clean_all
	-$(AT) cp ../include/vltPort.h.DUMMY ../include/vltPort.h
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean done"

man   : do_man
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

# GCH 2010.06.10
# Create needed directories.
# The ACS Makefile is capable of doing the complete job, but this
# VLT module comes first.
# In principle, the function should be moved from there to here!
# Now I put here only the directories needed for this vlt module, not
# the whole list.
CREATE_DIRS = include object doc bin lib config 

directory_structure:
	@echo "== Creating Missing directories"
	$(AT)for dir in $(foreach dir, $(CREATE_DIRS), $(dir)); \
	do \
		if [ ! -d ../$${dir} ]; then mkdir -p ../$${dir} ; fi; \
	done
#
#___oOo___

