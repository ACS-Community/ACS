#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: Makefile,v 1.97 2006/10/13 06:51:10 bjeram Exp $" 
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# almamgr 2002-03-01 Removed S_T generated file.
# gchiozzi 2002-01-17 Removed -I of TAO IFR include directory. Handled by acsMakefile.
# almamgr 2001-09-20 Fixed list of object files.
# almamgr 2001-09-20 Added target clean_dist
# bgustafs 2001-07-12 updated for maci 1.15, no debug
# bgustafs 2001-06-26 updated for maci 1.10
# almamgr 2000-07-20 created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------
LINK_FILES = $(addsuffix .cpp, $(ManagerShutdown_OBJECTS) $(Container_OBJECTS))

OPTIMIZE=0


# Program to build
# Can be overwritten in the call

PROG = 



#
# additional include and library search paths
#USER_LIB = 

#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = $(PROG) Container ManagerShutdown

EXECUTABLES_L   = 

#
# <brief description of xxxxx program>


Container_OBJECTS = maciContainer maciContainerImpl maciHelper maciContainerThreadHook \
                    maciLibraryManager maciServantManager maciORBTask maciContainerServices \
		    maciComponentStateManager
Container_LIBS = 

ManagerShutdown_OBJECTS = maciManagerShutdown 


$(PROG)_OBJECTS            = $(PROG)
$(PROG)_LDFLAGS            =
$(PROG)_LIBS               = $(TESTLIBS) C++ CCS

#
# special compilation flags for single c sources
#yyyyy_CFLAGS   = 

#
# Includes (.h) files (public and local)
# ---------------------------------
INCLUDES        = 
INCLUDES_L      = 
#
# Libraries (public and local)
# ----------------------------
LIBRARIES       =
LIBRARIES_L     =

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
# IDL FILES
#
IDL_FILES = 


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
# Default is UNIX, for VxVorks application next line MUST BE UNCOMMENTED
MAKE_VXWORKS = on   

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
all:	do_links do_all
	@echo " . . . 'all' done" 

clean : clean_links clean_all 
	$(RM) *~ ../include/*~ ../dbl/*~ ../idl/*~ ../*~ ../../*~ core
	@echo " . . . clean done"

clean_dist : clean clean_dist_all 
	@echo " . . . clean_dist done"

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

#___oOo___
