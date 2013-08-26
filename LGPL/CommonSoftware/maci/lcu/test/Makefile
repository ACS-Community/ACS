
# E.S.O. - ACS project
#
# "@(#) $Id: Makefile,v 1.82 2005/01/31 22:18:34 dfugate Exp $" 
#
# Makefile of maci
#
# who       when      what
# --------  --------  ----------------------------------------------
# bgustafs 2002-02-26 created
#

#*******************************************************************************
# This Makefile follows VLT Standards (see Makefile(5) for more).
#*******************************************************************************
# REMARKS
#    None
#------------------------------------------------------------------------

OPTIMIZE=0

# Program to build
# Can be overwritten in the call

PROG = 

#
# additional include and library search paths
USER_CFLAGS =
USER_LIB = 
#
# MODULE CODE DESCRIPTION:
# ------------------------
# As a general rule:  public file are "cleaned" and "installed"  
#                     local (_L) are not "installed".

#
# C programs (public and local)
# -----------------------------
EXECUTABLES     = maciTestClass


# ---> CDB_Test in text directory


#
# <brief description of xxxxx program>

maciTest_COMMON = maciTestClassImpl

maciTestClass_OBJECTS          = maciTestClassImpl maciTestC maciTestS

#
# Includes (.h) files (public and local)
# ---------------------------------
INCLUDES_L        =
INCLUDES        = 

# <brief description of tttttt tcl-script>
#
# TCL scripts (public and local)
# ------------------------------
TCL_SCRIPTS     =
TCL_SCRIPTS_L   =

#
tttttt_OBJECTS  =
tttttt_TCLSH    = 
tttttt_LIBS     = 

#
# libraries (public and local)
# ----------------------------
LIBRARIES       =
LIBRARIES_L     =

# baciTestUtils_OBJECTS   = baciTestUtils

#
# Scripts (public and local)
# ----------------------------
SCRIPTS           = 
SCRIPTS_L         = 


#
# TCL scripts (public and local)
# -----------------------------
TCL_SCRIPTS     = 
TCL_SCRIPTS_L   =


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
IDL_FILES = maciTest

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

ifdef WIND_BASE
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

man   : do_man 
	@echo " . . . man page(s) done"

install : install_all
	@echo " . . . installation done"

else
all:    
	@echo " WIND_BASE not defined. Nothing to do 'all'!"
clean : 
	@echo " WIND_BASE not defined. Nothing to 'clean' !"

clean_dist : 
	@echo " WIND_BASE not defined. Nothing to 'clean_dist' !"

man   : 
	@echo " WIND_BASE not defined. Nothing to 'man' !"

install : 
	@echo " WIND_BASE not defined. Nothing to 'install' !"

endif


#
# handle links to common source files (in slx/ws/src)
#
COMMON_SOURCES = \
	$(foreach exe, $(maciTest_COMMON), $(exe).cpp)
COMMON_SOURCES_ORIG = \
	$(foreach exe, $(maciTest_COMMON), ../../ws/test/$(exe).cpp)

$(COMMON_SOURCES) : $(COMMON_SOURCES_ORIG)
	- $(AT) if [ ! -h $@ ]; then echo "== Linking source: $@"; ln -s ../../ws/test/$@ $@; fi


COMMON_IDL = \
	$(foreach exe, $(IDL_FILES), ../idl/$(exe).idl)
COMMON_IDL_ORIG = \
	$(foreach exe, $(IDL_FILES), ../../ws/idl/$(exe).idl)

$(COMMON_IDL) : $(COMMON_IDL_ORIG)
	- $(AT) if [ ! -h $@ ]; then echo "== Linking source: $@"; ln -s ../../ws/idl/$@ $@; fi

do_links: $(COMMON_SOURCES) $(COMMON_IDL)

clean_links:
	$(AT) $(RM) $(COMMON_SOURCES) $(COMMON_IDL)
                 
#___oOo___
